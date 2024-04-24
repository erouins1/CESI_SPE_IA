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

library(dplyr)
library(ggplot2)

plot_percentiles <- function(data, data_scatter, predicted_weight, title, color) {
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
      geom_point(data = data_scatter, aes(y = Weight), color = "black", size = 1)+
      geom_point(data = predicted_weight, aes(y = Foetal_Weight), color = "red", size = 1)+
      # geom_line(aes(y = Weight), color = color, size = 1.2) +
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
      sliderInput("GestationalAgeSlider", "Gestational Age (week)", min = 15, max = 45, value = 38),
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 12, plotlyOutput("maternal_age_baby_weight")),
      box(width = 12, plotlyOutput("result_compare_plot")),
      box(width = 12, plotlyOutput("msePlot")), # Add this line to display MSE
      box(width = 12, plotlyOutput("c_p_model"))  # Add this line to display MSE
    )
  )
)

server <- function(input, output, session) {
  predicted_point <- reactive({
      # Check all inputs for NULL or missing values
      if(is.null(input$MaternalAgeSlider) || is.null(input$MaternalWeightSlider) ||
        is.null(input$MaternalHeightSlider) || is.null(input$MaternalParity) ||
        is.null(input$ChildGender) || is.null(input$GestationalAgeSlider)) {
        return(NULL)
      }
      # Predict weight using the model
      predicted_weight <- predict_foetal_weight(
        modelName = "./Models/p_c_model.rds",
        Maternal_age = input$MaternalAgeSlider,
        Maternal_weight = input$MaternalWeightSlider,
        Maternal_height = input$MaternalHeightSlider,
        Parity = input$MaternalParity,
        Sex = input$ChildGender,
        Gestational_age = input$GestationalAgeSlider  # Ensure this is singular or correctly handled if array
      )

      # Create data frame to return
      if (length(predicted_weight) == 0) {
        return(NULL)  # Return NULL if no prediction was made
      }

      data.frame(
        Gestational_age = input$GestationalAgeSlider,
        Foetal_Weight = exp(predicted_weight),  # Assuming output needs to be exponentiated
        Actual_Weight = load_data()$Weight  # Make sure load_data() reliably returns a compatible vector
      )
  })

  predicted_points <- reactive({
    data <- load_full_data()
    # Loop through the data and predict the foetal weight
    predicted_weight <- predict_foetal_weight("./Models/p_c_model.rds",
                                              data$Maternal_age,
                                              data$Maternal_weight,
                                              data$Maternal_height,
                                              data$Parity,
                                              data$Sex,
                                              data$Gestational_age)
    data.frame(
      Maternal_age = data$Maternal_age,
      Maternal_weight = data$Maternal_weight,
      Maternal_height = data$Maternal_height,
      Parity = data$Parity,
      Sex = data$Sex,
      Gestational_age = data$Gestational_age,  # Convert to weeks here for consistency
      Foetal_Weight = exp(predicted_weight),
      Actual_Weight = data$Weight,  # Assume this matches by index/order
      Residuals = (data$Weight - predicted_weight))  # Squared residuals
  })

  mse_by_age <- reactive({
    df <- predicted_points()
    df %>%
      group_by(Gestational_age) %>%
      summarise(RMSE = sqrt(mean(Residuals, na.rm = TRUE)), .groups = 'drop')  # Calculate MSE per gestational age group
  })

  # Plot the MSE compared to the gestational age
  output$msePlot <- renderPlotly({
    mse_data <- mse_by_age()  # Access the reactive MSE data

    # Create the MSE plot
    mse_plot <- plot_ly(data = mse_data, x = ~Gestational_age, y = ~RMSE,
                        type = 'scatter', mode = 'line+markers',
                        marker = list(color = 'rgba(255, 65, 54, 0.8)')) %>%
      layout(title = "RMSE by Gestational Age",
             xaxis = list(title = "Gestational Age (weeks)"),
             yaxis = list(title = "RMSE"))

    return(mse_plot)
  })

  processed_data <- reactive({
    req(load_full_data())  # Ensure data is loaded
    load_full_data() %>%
      mutate(Gestational_Age_Group = cut(Gestational_age, breaks = c(12, 24, 36, 40), labels = c("First Trimester", "Second Trimester", "Third Trimester")),
             Maternal_Age_Quartile = ntile(Maternal_age, 4))
  })

  outcomes_by_group <- reactive({
    req(processed_data())  # Ensure processed data is ready
    processed_data() %>%
      group_by(Gestational_Age_Group, Maternal_Age_Quartile) %>%
      summarise(Weight = mean(Weight, na.rm = TRUE), .groups = 'drop')
  })

  # Use plot percentiles function to create the plot
  output$maternal_age_baby_weight <- renderPlotly({
    # Ensure data is available
    req(outcomes_by_group())
    df <- outcomes_by_group()

    # Check if df is empty
    if(nrow(df) == 0) {
      return(NULL)  # If no data, return nothing
    }

    # Ensure columns are not factors (as this could be part of the problem)
    df$Gestational_Age_Group <- as.character(df$Gestational_Age_Group)
    df$Maternal_Age_Quartile <- as.character(df$Maternal_Age_Quartile)

    # Try creating the plot without dynamic colors first to ensure basic functionality
    plot <- plot_ly(data = df, x = ~Gestational_Age_Group, y = ~Weight, type = 'bar', color = ~Maternal_Age_Quartile) %>%
      layout(title = "Mean Outcome by Gestational Age Group and Weight Quartile",
             xaxis = list(title = "Gestational Age Group"),
             yaxis = list(title = "Mean Outcome"),
             barmode = 'group')

    # Return the plot
    return(plot)
  })
  # import du modèle p_c_model.rds*
  model_p_c <- readRDS("./Models/p_c_model.rds")

  output$c_p_model <- renderPlotly({
    predicted_df <- predicted_point()
    all_data <- load_full_data()
    data <- all_data

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


    p <- plot_percentiles(all_data, data, predicted_df, "Polynomial Model: Growth Percentiles", "black")

    # Return the plot
    p
  })

  output$result_compare_plot <- renderPlotly({
    predicted_points <- predicted_points()
    p <- ggplot(predicted_points, aes(x = Actual_Weight, y = Foetal_Weight)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      ggtitle("Mixed Linear Model: Predicted vs Actual Weights") +
      xlab("Actual Weight") +
      ylab("Predicted Weight")
  })
}

shinyApp(ui = ui, server = server)
