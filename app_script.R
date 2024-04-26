library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(splines)
library(zoo)
library(Metrics)

load_data <- function() {
  data <- read_csv("TreatedData/test_set.csv")
  return(data)
}

load_full_data <- function() {
  full_data <- read_csv("TreatedData/data.csv")

  return(full_data)
}

GroupData <- function (Gestational_age) {
  Group <- ifelse(Gestational_age < 23, 1, ifelse(Gestational_age < 34, 2, ifelse(Gestational_age < 37, 3, 4)))
  Group <- as.factor(Group)
  return(Group)
}

predict_foetal_weight_poly <- function(Maternal_age, Maternal_weight, Maternal_height, Parity, Sex, Gestational_age) {
  model <- readRDS("./Models/p_c_model.rds")


  predictions <- vector("numeric", length(Gestational_age))
  for (i in seq_along(Gestational_age)) {
    data <- data.frame(
      Maternal_age = as.numeric(Maternal_age[i]),
      Maternal_weight = as.numeric(Maternal_weight[i]),
      Maternal_height = as.numeric(Maternal_height[i]),
      Parity = as.numeric(Parity[i]),
      Sex = as.factor(Sex[i]),
      Gestational_age = Gestational_age[i]
    )
    predictions[i] <- predict(model, newdata = data)
  }
  return(predictions)
}

predict_foetal_weight_linear <- function(Maternal_age, Maternal_weight, Maternal_height, Parity, Sex, Gestational_age) {
  model <- readRDS("./Models/linear_model.rds")
  if (length(Gestational_age) == 0) {
    stop("Gestational_ages is missing or empty.")
  }

  predictions <- vector("numeric", length(Gestational_age))
  for (i in seq_along(Gestational_age)) {

    if (is.null(Maternal_age[i]) || is.null(Maternal_weight[i]) || is.null(Maternal_height[i]) || is.null(Parity[i]) || is.null(Sex[i]))  {
      stop("One of the required parameters is missing a value at index: ", i)
    }
    data <- data.frame(
      Maternal_age = as.numeric(Maternal_age[i]),
      Maternal_weight = as.numeric(Maternal_weight[i]),
      Maternal_height = as.numeric(Maternal_height[i]),
      Parity = as.numeric(Parity[i]),
      Sex = as.factor(Sex[i]),
      Gestational_age = Gestational_age[i]
    )
    predictions[i] <- predict(model, newdata = data)
  }
  return(predictions)
}


library(dplyr)
library(ggplot2)

plot_percentiles <- function(data, data_scatter, predicted_weight, title, color) {

  if("Weight_Predicted" %in% names(data)) {
    data %>%
      group_by(Gestational_age) %>%
      summarise(
        Weight = mean(Weight_Predicted, na.rm = TRUE),
        Percentile1 = quantile(Weight_Predicted, 0.01, na.rm = TRUE),
        Percentile3 = quantile(Weight_Predicted, 0.03, na.rm = TRUE),
        Percentile10 = quantile(Weight_Predicted, 0.1, na.rm = TRUE),
        Percentile90 = quantile(Weight_Predicted, 0.9, na.rm = TRUE),
        Percentile97 = quantile(Weight_Predicted, 0.97, na.rm = TRUE),
        Percentile99 = quantile(Weight_Predicted, 0.99, na.rm = TRUE)
      ) %>%
      ggplot(aes(x = Gestational_age)) +
      geom_point(data = data_scatter, aes(y = Weight), color = "black", size = 1)+
      geom_point(data = predicted_weight, aes(y = Foetal_Weight), color = "red", size = 1)+
      # geom_line(aes(y = Weight), color = color, size = 1.2) +
      geom_line(aes(y = Percentile1), color = "pink", linetype = "dashed") +
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

calculate_r2 <- function(actual, predicted) {

  ss_res <- sum((actual - predicted) ^ 2)

  ss_tot <- sum((actual - mean(actual)) ^ 2)

  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}

count_extreme_percentiles <- function(percentile_data, count_data) {

  if (!is.numeric(percentile_data) || !is.numeric(count_data)) {
    stop("Both inputs must be numeric vectors.")
  }

  p10 <- quantile(percentile_data, 0.1)
  p90 <- quantile(percentile_data, 0.9)

  count_below_p10 <- sum(count_data < p10)
  count_above_p90 <- sum(count_data > p90)

  total_count <- length(count_data)

  percent_below_p10 <- (count_below_p10 / total_count) * 100
  percent_above_p90 <- (count_above_p90 / total_count) * 100

  return(list(below_10th_percentile = percent_below_p10,
              above_90th_percentile = percent_above_p90))
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
      selectInput("MaternalParity", "Maternal Parity", choices = c(0, 1, 2, 3, 4, 5)),
      selectInput("ChildGender", "Child Gender", choices = c("Male", "Female")),
      sliderInput("GestationalAgeSlider", "Gestational Age (week)", min = 15, max = 45, value = 38),
    )
  ),
  dashboardBody(
    fluidRow(
      width= 12,
      box(title = "R^2 Score Polynomial Model", status = "primary", solidHeader = TRUE, width = 3, textOutput("r2Poly")),
      box(title = "Mean Absolute Error Polynomial Model", status = "primary", solidHeader = TRUE, width = 3, textOutput("maePoly")),
      box(title = "R^2 Score Linear Model", status = "primary", solidHeader = TRUE, width = 3, textOutput("r2Linear")),
          box(title = "Mean Absolute Error Linear Model", status = "primary", solidHeader = TRUE, width = 3, textOutput("maeLinear")),
    ),
    fluidRow(
      width = 12,
        box(width = 6, plotlyOutput("c_p_model")),
        box(width = 6, plotlyOutput("linear_model"))
    ),
    fluidRow(
        box(width = 6, textOutput("OverExtremesPoly")),
        box(width = 6, textOutput("OverExtremesLinear"))
    ),
    fluidRow(
      width = 12,
      box(width = 6, plotlyOutput("result_compare_plot_poly")),
      box(width = 6, plotlyOutput("result_compare_plot_linear")),
    ),
    fluidRow(
      width = 12,
      box(width = 4, plotlyOutput("maternal_age_baby_weight")),
      box(width = 4, plotlyOutput("residualsPlotLinear")),
      box(width = 4, plotlyOutput("residualsPlotPoly")),
    ),
  )
)

server <- function(input, output, session) {
  predicted_point_poly <- reactive({

      if(is.null(input$MaternalAgeSlider) || is.null(input$MaternalWeightSlider) ||
        is.null(input$MaternalHeightSlider) || is.null(input$MaternalParity) ||
        is.null(input$ChildGender) || is.null(input$GestationalAgeSlider)) {
        return(NULL)
      }

      predicted_weight <- predict_foetal_weight_poly(
        Maternal_age = input$MaternalAgeSlider,
        Maternal_weight = input$MaternalWeightSlider,
        Maternal_height = input$MaternalHeightSlider,
        Parity = input$MaternalParity,
        Sex = input$ChildGender,
        Gestational_age = input$GestationalAgeSlider
      )


      if (length(predicted_weight) == 0) {
        return(NULL)
      }

      data.frame(
        Gestational_age = input$GestationalAgeSlider,
        Foetal_Weight = exp(predicted_weight),
        Actual_Weight = load_data()$Weight
      )
  })

  predicted_point_linear <- reactive({

    if(is.null(input$MaternalAgeSlider) || is.null(input$MaternalWeightSlider) ||
      is.null(input$MaternalHeightSlider) || is.null(input$MaternalParity) ||
      is.null(input$ChildGender) || is.null(input$GestationalAgeSlider)) {
      return(NULL)
    }

    predicted_weight <- predict_foetal_weight_linear(
      Maternal_age = input$MaternalAgeSlider,
      Maternal_weight = input$MaternalWeightSlider,
      Maternal_height = input$MaternalHeightSlider,
      Parity = input$MaternalParity,
      Sex = input$ChildGender,
      Gestational_age = input$GestationalAgeSlider
    )


    if (length(predicted_weight) == 0) {
      return(NULL)
    }

    data.frame(
      Gestational_age = input$GestationalAgeSlider,
      Foetal_Weight = exp(predicted_weight),
      Actual_Weight = load_data()$Weight
    )
  })

  predicted_points_poly <- reactive({
    data <- load_full_data()

    predicted_weight <- predict_foetal_weight_poly(
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
      Gestational_age = data$Gestational_age,
      Foetal_Weight = exp(predicted_weight),
      Actual_Weight = data$Weight,
      Residuals = (data$Weight - exp(predicted_weight)))
  })

  predicted_points_linear <- reactive({
    data <- load_full_data()
    # Loop through the data and predict the foetal weight
    predicted_weight <- predict_foetal_weight_linear(
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
      Gestational_age = data$Gestational_age,
      Foetal_Weight = exp(predicted_weight),
      Actual_Weight = data$Weight,
      Residuals = (data$Weight - exp(predicted_weight)))
  })

  output$residualsPlotPoly <- renderPlotly({
    df <- predicted_points_poly()
    if (is.null(df)) {
      return(NULL)
    }

    plot <- plot_ly(data = df, x = ~Gestational_age, y = ~Residuals,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'rgba(255, 65, 54, 0.8)')) %>%
      layout(title = "Residuals by Gestational Age for Polynomial Model",
             xaxis = list(title = "Gestational Age (weeks)"),
             yaxis = list(title = "Residuals"))

    return(plot)
  })

  # Plot residuals for linear model
  output$residualsPlotLinear <- renderPlotly({
    df <- predicted_points_linear()
    if (is.null(df)) {
      return(NULL)
    }

    plot <- plot_ly(data = df, x = ~Gestational_age, y = ~Residuals,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'rgba(255, 65, 54, 0.8)')) %>%
      layout(title = "Residuals by Gestational Age for Linear Model",
             xaxis = list(title = "Gestational Age (weeks)"),
             yaxis = list(title = "Residuals"))

    return(plot)
  })

  processed_data <- reactive({
    req(load_full_data())
    load_full_data() %>%
      mutate(Gestational_Age_Group = cut(Gestational_age, breaks = c(0, 22, 33, 36, 45), labels = c("0-22", "23-33", "34-36", "37+")),
             Maternal_Age_Quartile = ntile(Maternal_age, 4))
  })

  outcomes_by_group <- reactive({
    req(processed_data())
    processed_data() %>%
      group_by(Gestational_Age_Group, Maternal_Age_Quartile) %>%
      summarise(Weight = mean(Weight, na.rm = TRUE), .groups = 'drop')
  })


  output$maternal_age_baby_weight <- renderPlotly({

    req(outcomes_by_group())
    df <- outcomes_by_group()


    if(nrow(df) == 0) {
      return(NULL)
    }


    df$Gestational_Age_Group <- as.character(df$Gestational_Age_Group)
    df$Maternal_Age_Quartile <- as.character(df$Maternal_Age_Quartile)

    # Try creating the plot without dynamic colors first to ensure basic functionality
    plot <- plot_ly(data = df, x = ~Gestational_Age_Group, y = ~Weight, type = 'bar', color = ~Maternal_Age_Quartile) %>%
      layout(title = "Mean Weight by Gestational Age Group and Maternal Age Quartile",
             xaxis = list(title = "Gestational Age Group"),
             yaxis = list(title = "Mean Weight"),
             barmode = 'group')


    return(plot)
  })

  model_p_c <- readRDS("./Models/p_c_model.rds")
  model_linear <- readRDS("./Models/linear_model.rds")

  output$c_p_model <- renderPlotly({
    predicted_df <- predicted_point_poly()
    all_data <- load_full_data()
    data <- all_data

    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)

    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex),
      Group = unique(all_data$Group))

    all_data$Weight_Predicted <- exp(predict(model_p_c, newdata = all_data))


    p <- plot_percentiles(all_data, data, predicted_df, "Polynomial Model: Growth Percentiles", "black")


    p
  })

  output$linear_model <- renderPlotly({
    predicted_df <- predicted_point_linear()
    all_data <- load_full_data()
    data <- all_data

    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)

    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex))

    all_data$Weight_Predicted <- exp(predict(model_linear, newdata = all_data))

    p <- plot_percentiles(all_data, data, predicted_df, "Linear Model: Growth Percentiles", "black")

    p
  })

  output$result_compare_plot_poly <- renderPlotly({
    predicted_points <- predicted_points_poly()
    p <- ggplot(predicted_points, aes(x = Actual_Weight, y = Foetal_Weight)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      ggtitle("Predicted vs Actual Weights using polynomial model") +
      xlab("Actual Weight") +
      ylab("Predicted Weight")
  })

  output$result_compare_plot_linear <- renderPlotly({
    predicted_points <- predicted_points_linear()
    p <- ggplot(predicted_points, aes(x = Actual_Weight, y = Foetal_Weight)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      ggtitle("Predicted vs Actual Weights using linear model") +
      xlab("Actual Weight") +
      ylab("Predicted Weight")
  })


  r2_poly <- reactive({
    predicted_points <- predicted_points_poly()
    if (is.null(predicted_points)) {
      return(NA)
    }
    actual <- predicted_points$Actual_Weight
    predicted <- predicted_points$Foetal_Weight
    calculate_r2(actual, predicted)
  })


  r2_linear <- reactive({
    predicted_points <- predicted_points_linear()
    if (is.null(predicted_points)) {
      return(NA)
    }
    actual <- predicted_points$Actual_Weight
    predicted <- predicted_points$Foetal_Weight
    calculate_r2(actual, predicted)
  })


  output$r2Poly <- renderText({
    r2_score <- r2_poly()
    if (is.na(r2_score)) {
      "R^2 Score: N/A"
    } else {
      paste("R^2 Score for Polynomial Model:", format(r2_score, digits = 4))
    }
  })

  # Output R^2 score for linear model
  output$r2Linear <- renderText({
    r2_score <- r2_linear()
    if (is.na(r2_score)) {
      "R^2 Score: N/A"
    } else {
      paste("R^2 Score for Linear Model:", format(r2_score, digits = 4))
    }
  })

  output$maePoly <- renderText({
    predicted_points <- predicted_points_poly()
    if (is.null(predicted_points)) {
      return(NA)
    }
    actual <- predicted_points$Actual_Weight
    predicted <- predicted_points$Foetal_Weight

    paste("MAE Score for Polynomial Model:", format(mae(actual, predicted), digits = 4))
  })

    output$maeLinear <- renderText({
        predicted_points <- predicted_points_linear()
        if (is.null(predicted_points)) {
        return(NA)
        }
        actual <- predicted_points$Actual_Weight
        predicted <- predicted_points$Foetal_Weight

        paste("MAE Score for Linear Model:", format(mae(actual, predicted), digits = 4))
    })

  output$OverExtremesPoly <- renderText({
    all_data <- load_full_data()
    data <- all_data

    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)

    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex))

    all_data$Weight_Predicted <- exp(predict(model_p_c, newdata = all_data))

    count_extremes <- count_extreme_percentiles(all_data$Weight_Predicted, all_data$Weight)

    paste("Percentage of babies below 10th percentile: ", count_extremes$below_10th_percentile, "\n",
          "Percentage of babies above 90th percentile: ", count_extremes$above_90th_percentile)
  })

  output$OverExtremesLinear <- renderText({
    all_data <- load_full_data()
    data <- all_data

    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)

    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex))

    all_data$Weight_Predicted <- exp(predict(model_linear, newdata = all_data))

    count_extremes <- count_extreme_percentiles(all_data$Weight_Predicted, all_data$Weight)

    paste("Percentage of babies below 10th percentile: ", count_extremes$below_10th_percentile, "\n",
          "Percentage of babies above 90th percentile: ", count_extremes$above_90th_percentile)
  })

}

shinyApp(ui = ui, server = server)
