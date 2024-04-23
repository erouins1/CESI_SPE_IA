library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(splines)
library(zoo)

# Function to pull the data from the TreatedData/test_set.csv file into a global variable
load_data <- function() {
  data <- read_csv("TreatedData/test_set.csv")
  return(data)
}

predict_foetal_weight <- function(modelName, Maternal_age, Maternal_weight, Maternal_height, Parity, Sex, Gestational_ages) {
  model <- readRDS(modelName)

  predictions <- vector("numeric", length(Gestational_ages))  # Pre-allocate the vector
  for (i in seq_along(Gestational_ages)) {
    data <- data.frame(
      Maternal_age = as.numeric(Maternal_age[i]),
      Maternal_weight = as.numeric(Maternal_weight[i]),
      Maternal_height = as.numeric(Maternal_height[i]),
      Parity = as.numeric(Parity[i]),
      Sex = as.factor(Sex[i]),
      Gestational_age = Gestational_ages[i]
    )
    predictions[i] <- predict(model, newdata = data)
  }
  return(predictions)
}

# Calculates percentiles of weight per Gestational_age brackets for the data frame
calculate_percentiles <- function(data) {
  percentiles <- data %>%
    group_by(Gestational_age) %>%
    summarise(
      P1 = quantile(Weight, 0.01, na.rm = TRUE),
      P3 = quantile(Weight, 0.03, na.rm = TRUE),
      P10 = quantile(Weight, 0.10, na.rm = TRUE),
      P90 = quantile(Weight, 0.90, na.rm = TRUE),
      P97 = quantile(Weight, 0.97, na.rm = TRUE),
      P99 = quantile(Weight, 0.99, na.rm = TRUE),
      .groups = 'keep'  # Ensures that the grouping variables are kept
    )
  return(percentiles)
}

smooth_percentiles <- function(percentiles){
  smooth_percentiles <- data.frame(
    Gestational_age = percentiles$Gestational_age,
    P1 = lowess(percentiles$Gestational_age, percentiles$P1, f = 0.25)$y,
    P3 = lowess(percentiles$Gestational_age, percentiles$P3, f = 0.25)$y, # Adjust f for smoothing span
    P10 = lowess(percentiles$Gestational_age, percentiles$P10, f = 0.25)$y,
    P90 = lowess(percentiles$Gestational_age, percentiles$P90, f = 0.25)$y,
    P97 = lowess(percentiles$Gestational_age, percentiles$P97, f = 0.25)$y,
    P99 = lowess(percentiles$Gestational_age, percentiles$P99, f = 0.25)$y
  )
  return(smooth_percentiles)
}

plot_percentiles <- function(data, weight_col, title, color) {
  data %>%
    group_by(Gestational_age) %>%
    summarise(
      Weight = mean(!!sym(weight_col), na.rm = TRUE),
      Percentile3 = quantile(!!sym(weight_col), 0.03, na.rm = TRUE),
      Percentile10 = quantile(!!sym(weight_col), 0.1, na.rm = TRUE),
      Percentile90 = quantile(!!sym(weight_col), 0.9, na.rm = TRUE),
      Percentile97 = quantile(!!sym(weight_col), 0.97, na.rm = TRUE),
      Percentile99 = quantile(!!sym(weight_col), 0.99, na.rm = TRUE)
    ) %>%
    ggplot(aes(x = Gestational_age)) +
    geom_line(aes(y = Weight), color = color, size = 1.2) +
    geom_line(aes(y = Percentile3), color = "blue", linetype = "dashed") +
    geom_line(aes(y = Percentile10), color = "green", linetype = "dashed") +
    geom_line(aes(y = Percentile90), color = "orange", linetype = "dashed") +
    geom_line(aes(y = Percentile97), color = "red", linetype = "dashed") +
    geom_line(aes(y = Percentile99), color = "purple", linetype = "dashed") +
    labs(title = title, y = "Predicted Weight", x = "Gestational Age")
}


ui <- dashboardPage(
  dashboardHeader(title = "Foetal Weight Prediction"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .sidebar .box-header .box-title {
          text-align: center;
          display: block;
        }
        .form-group > label {
          color: black;
          visibility: visible;
          display: block;
          text-align: center;
        }
      "))
    ),
    box(
      width = "100%",
      title = "Input Parameters",
      sliderInput("MaternalAgeSlider", "Maternal Age (years)", min = 14, max = 60, value = 30),
      sliderInput("MaternalWeightSlider", "Maternal Weight (kg)", min = 40, max = 150, value = 70),
      sliderInput("MaternalHeightSlider", "Maternal Height (cm)", min = 120, max = 230, value = 165),
      selectInput("MaternalParity", "Maternal Parity", choices = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)),
      selectInput("ChildGender", "Child Gender", choices = c("Male", "Female")),
      sliderInput("GestationalAgeSlider", "Gestational Age (days)", min = 120, max = 320, value = 270),
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 12, plotlyOutput("plot")),
      box(width = 12, plotlyOutput("result_compare_plot"))
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    read_csv("TreatedData/test_set.csv")
  })

  reactive_lowess <- reactive({ # Accessing reactive data
    lowess(data()$Gestational_age, data()$Weight)   # LOWESS smoothing
  })

  predicted_point <- reactive({
    predicted_weight <- predict_foetal_weight("./Models/polynomial_model.rds",
                                              input$MaternalAgeSlider,
                                              input$MaternalWeightSlider,
                                              input$MaternalHeightSlider,
                                              input$MaternalParity,
                                              input$ChildGender,
                                              input$GestationalAgeSlider)
    data.frame(Gestational_age = floor(input$GestationalAgeSlider / 7),  # Convert to weeks here for consistency
               Foetal_Weight = predicted_weight,
               Actual_Weight = data()$Weight)  # Assume this matches by index/order
  })

  predicted_points <- reactive({
    # Loop through the data and predict the foetal weight
    predicted_weight <- predict_foetal_weight("./Models/polynomial_model.rds",
                                              data()$Maternal_age,
                                              data()$Maternal_weight,
                                              data()$Maternal_height,
                                              data()$Parity,
                                              data()$Sex,
                                              data()$Gestational_age)
    data.frame(
      Maternal_age = data()$Maternal_age, Maternal_weight = data()$Maternal_weight, Maternal_height = data()$Maternal_height, Parity = data()$Parity, Sex = data()$Sex,
      Gestational_age = floor(data()$Gestational_age / 7),  # Convert to weeks here for consistency
      Foetal_Weight = predicted_weight,
      Actual_Weight = data()$Weight)  # Assume this matches by index/order
  })

  percentiles <- reactive({
    df <- data()  # Make sure to call the reactive function to get the current data
    calculate_percentiles(df)
  })

  smoothed_percentiles <- reactive({
    percentiles <- percentiles()  # Access the reactive percentiles
    smooth_percentiles(percentiles)
  })

  # Use plot percentiles function to create the plot
  output$plot <- renderPlotly({
    data <- data()  # Access reactive data
    smoothed_percentiles <- smoothed_percentiles()  # Access reactive smoothed percentiles
    req(smoothed_percentiles)  # Ensure the percentiles are computed before plotting

    ggplot(smoothed_percentiles, aes(x = Gestational_age)) +
      geom_line(aes(y = P1, color = "1%")) +
      geom_line(aes(y = P10, color = "10%")) +
      geom_line(aes(y = P90, color = "90%")) +
      geom_line(aes(y = P99, color = "99%")) +
      scale_color_manual(values = c("1%" = "red", "10%" = "blue", "90%" = "green", "99%" = "purple")) +
      labs(title = "Smoothed Percentiles of Weight by Gestational Age", y = "Weight", x = "Gestational Age") +
      theme_minimal()

    # ggplot(data, aes(x = Gestational_age, y = Weight)) +
    #1 geom_point() +
    # geom_line(data = data.frame(x = lowess_results$x, y = lowess_results$y), aes(x = x, y = y), color = "blue") +
    #  labs(title = "LOWESS Smoothing", x = "X Axis", y = "Y Axis")
  })

  # Create a residual plot
  output$result_compare_plot <- renderPlotly({
    predicted_df <- predicted_points()

    # Calculate residuals
    residuals <- predicted_df$Actual_Weight - predicted_df$Foetal_Weight

    # Create the residual plot
    residual_plot <- plot_ly(data = predicted_df, x = ~Gestational_age, y = ~residuals,
                             type = 'scatter', mode = 'markers',
                             marker = list(color = 'rgba(255, 65, 54, 0.8)')) %>%
      layout(title = "Residuals Plot",
             xaxis = list(title = "Predicted Foetal Weight"),
             yaxis = list(title = "Residuals (Actual Weight - Predicted Weight)"))

    return(residual_plot)
  })
}

shinyApp(ui = ui, server = server)
