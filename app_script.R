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

load_full_data <- function() {
  full_data <- read_csv("TreatedData/data.csv")

  return(full_data)
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
      P1 = quantile(Weight, 0.01, na.rm = TRUE, type = 3),
      P3 = quantile(Weight, 0.03, na.rm = TRUE, type = 3),
      P10 = quantile(Weight, 0.10, na.rm = TRUE, type = 3),
      P90 = quantile(Weight, 0.90, na.rm = TRUE, type = 3),
      P97 = quantile(Weight, 0.97, na.rm = TRUE, type = 3),
      P99 = quantile(Weight, 0.99, na.rm = TRUE, type = 3),
      .groups = 'keep'  # Ensures that the grouping variables are kept
    )
  return(percentiles)
}

library(dplyr)
library(ggplot2)

plot_percentiles <- function(data, title, color) {
  # Ensure that data is a dataframe and Weight_Poly_C is present as a column
  if("Weight_Poly_C" %in% names(data)) {
    data %>%
      group_by(Gestational_age) %>%
      summarise(
        Weight = mean(Weight_Poly_C, na.rm = TRUE),  # Calculate mean weight
        Percentile3 = quantile(Weight_Poly_C, 0.03, na.rm = TRUE),  # 3rd percentile
        Percentile10 = quantile(Weight_Poly_C, 0.1, na.rm = TRUE),  # 10th percentile
        Percentile90 = quantile(Weight_Poly_C, 0.9, na.rm = TRUE),  # 90th percentile
        Percentile97 = quantile(Weight_Poly_C, 0.97, na.rm = TRUE),  # 97th percentile
        Percentile99 = quantile(Weight_Poly_C, 0.99, na.rm = TRUE)   # 99th percentile
      ) %>%
      ggplot(aes(x = Gestational_age)) +
      geom_line(aes(y = Weight), color = color, size = 1.2) +
      geom_line(aes(y = Percentile3), color = "blue", linetype = "dashed") +
      geom_line(aes(y = Percentile10), color = "green", linetype = "dashed") +
      geom_line(aes(y = Percentile90), color = "orange", linetype = "dashed") +
      geom_line(aes(y = Percentile97), color = "red", linetype = "dashed") +
      geom_line(aes(y = Percentile99), color = "purple", linetype = "dashed") +
      labs(title = title, y = "Predicted Weight", x = "Gestational Age")
  } else {
    stop("Weight_Poly_C column not found in the provided data.")
  }
}

smooth_percentiles <- function(percentiles){
  smooth_percentiles <- data.frame(
    Gestational_age = percentiles$Gestational_age,
    P1 = lowess(percentiles$Gestational_age, percentiles$P1, f = 0.15)$y,
    P3 = lowess(percentiles$Gestational_age, percentiles$P3, f = 0.15)$y, # Adjust f for smoothing span
    P10 = lowess(percentiles$Gestational_age, percentiles$P10, f = 0.15)$y,
    P90 = lowess(percentiles$Gestational_age, percentiles$P90, f = 0.15)$y,
    P97 = lowess(percentiles$Gestational_age, percentiles$P97, f = 0.15)$y,
    P99 = lowess(percentiles$Gestational_age, percentiles$P99, f = 0.15)$y
  )
  return(smooth_percentiles)
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
      box(width = 12, plotlyOutput("result_compare_plot")),
      box(width = 12, plotlyOutput("msePlot")), # Add this line to display MSE
      box(width = 12, plotlyOutput("c_p_model"))  # Add this line to display MSE
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    read_csv("TreatedData/test_set.csv")
  })

  all_data <- reactive({
    read_csv("TreatedData/data.csv")
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
      Maternal_age = data()$Maternal_age,
      Maternal_weight = data()$Maternal_weight,
      Maternal_height = data()$Maternal_height,
      Parity = data()$Parity,
      Sex = data()$Sex,
      Gestational_age = floor(data()$Gestational_age / 7),  # Convert to weeks here for consistency
      Foetal_Weight = predicted_weight,
      Actual_Weight = data()$Weight,  # Assume this matches by index/order
      Residuals = (data()$Weight - predicted_weight)^2)  # Squared residuals
  })

  percentiles <- reactive({
    df <- data()  # Make sure to call the reactive function to get the current data
    calculate_percentiles(df)
  })

  smoothed_percentiles <- reactive({
    percentiles <- percentiles()  # Access the reactive percentiles
    smooth_percentiles(percentiles)
  })

  mse_by_age <- reactive({
    df <- predicted_points()
    df %>%
      group_by(Gestational_age) %>%
      summarise(MSE = mean(Residuals, na.rm = TRUE), .groups = 'drop')  # Calculate MSE per gestational age group
  })

  # Plot the MSE compared to the gestational age
  output$msePlot <- renderPlotly({
    mse_data <- mse_by_age()  # Access the reactive MSE data

    # Create the MSE plot
    mse_plot <- plot_ly(data = mse_data, x = ~Gestational_age, y = ~MSE,
                        type = 'scatter', mode = 'line+markers',
                        marker = list(color = 'rgba(255, 65, 54, 0.8)')) %>%
      layout(title = "Mean Squared Error (MSE) by Gestational Age",
             xaxis = list(title = "Gestational Age (weeks)"),
             yaxis = list(title = "Mean Squared Error (MSE)"))

    return(mse_plot)
  })

  # Use plot percentiles function to create the plot
  output$plot <- renderPlotly({
    data <- data()  # Access reactive data
    percentiles <- percentiles()  # Access reactive smoothed percentiles
    req(percentiles)  # Ensure the percentiles are computed before plotting

    p <- ggplot() +
      geom_point(data = data, aes(x = Gestational_age, y = Weight, color = "Data Points"), size = 3, show.legend = TRUE) +  # Scatter plot first
      geom_line(data = percentiles, aes(x = Gestational_age, y = P1, color = "1%")) +
      geom_line(data = percentiles, aes(x = Gestational_age, y = P3, color = "3%")) +
      geom_line(data = percentiles, aes(x = Gestational_age, y = P10, color = "10%")) +
      geom_line(data = percentiles, aes(x = Gestational_age, y = P90, color = "90%")) +
      geom_line(data = percentiles, aes(x = Gestational_age, y = P97, color = "97%")) +
      geom_line(data = percentiles, aes(x = Gestational_age, y = P99, color = "99%")) +
      scale_color_manual(values = c("Data Points" = "black", "1%" = "red", "3%" = "orange", "10%" = "blue", "90%" = "green", "97%" = "pink", "99%" = "purple")) +
      labs(title = "Smoothed Percentiles of Weight by Gestational Age", y = "Weight", x = "Gestational Age") +
      theme_minimal()

    # Return the plot
    p
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
  # import du modèle p_c_model.rds*
  model_p_c <- readRDS("./Models/p_c_model.rds")

  output$c_p_model <- renderPlotly({

    all_data <- load_full_data()
    print(all_data)

    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)

    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex),
      Group = unique(all_data$Group))

    print(all_data)

    all_data$Weight_Poly_C <- exp(predict(model_p_c, newdata = all_data))


    p <- plot_percentiles(all_data,"Polynomial Model: Growth Percentiles", "black")

    # Return the plot
    p
  })
  

  
}

shinyApp(ui = ui, server = server)
