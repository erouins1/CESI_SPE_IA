library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)

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
    mutate(Week = floor(Gestational_age / 7)) %>%  # Convert days to weeks
    group_by(Week) %>%
    summarise(
      P1 = quantile(Weight, 0.01, na.rm = TRUE),
      P3 = quantile(Weight, 0.03, na.rm = TRUE),
      P10 = quantile(Weight, 0.10, na.rm = TRUE),
      P90 = quantile(Weight, 0.90, na.rm = TRUE),
      P97 = quantile(Weight, 0.97, na.rm = TRUE),
      P99 = quantile(Weight, 0.99, na.rm = TRUE)
    )
  return(percentiles)
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
      box(width = 12, plotlyOutput("plot"))
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    read_csv("TreatedData/test_set.csv")
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
               Foetal_Weight = predicted_weight)
  })

  percentiles <- reactive({
    calculate_percentiles(data())
  })

  # Observe percentiles
  observe({
    print(colnames(percentiles()))  # Debugging line to check column names
    print(head(percentiles()))      # Print first few rows to check data
  })

  output$plot <- renderPlotly({
    df <- data()
    predicted_df <- predicted_point()
    percentile_df <- percentiles()  # Get the percentile data

    plot <- plot_ly() %>%
      add_trace(data = df, x = ~floor(Gestational_age / 7), y = ~Weight,
                type = 'scatter', mode = 'markers', name = 'Actual Data',
                marker = list(color = 'blue')) %>%
      add_trace(data = predicted_df, x = ~Gestational_age, y = ~Foetal_Weight,
                type = 'scatter', mode = 'markers', name = 'Predicted Weight',
                marker = list(color = 'red', size = 10))

    # Add lines for each percentile
    plot <- plot %>%
      add_lines(data = percentile_df, x = ~Week, y = ~P1, name = '1st Percentile', line = list(color = 'orange', dash = 'dot')) %>%
      add_lines(data = percentile_df, x = ~Week, y = ~P3, name = '3rd Percentile', line = list(color = 'yellow', dash = 'dot')) %>%
      add_lines(data = percentile_df, x = ~Week, y = ~P10, name = '10th Percentile', line = list(color = 'green', dash = 'dot')) %>%
      add_lines(data = percentile_df, x = ~Week, y = ~P90, name = '90th Percentile', line = list(color = 'purple', dash = 'dot')) %>%
      add_lines(data = percentile_df, x = ~Week, y = ~P97, name = '97th Percentile', line = list(color = 'brown', dash = 'dot')) %>%
      add_lines(data = percentile_df, x = ~Week, y = ~P99, name = '99th Percentile', line = list(color = 'black', dash = 'dot'))

    plot <- plot %>% layout(title = "Foetal Weight by Gestational Week",
                            xaxis = list(title = "Gestational Week"),
                            yaxis = list(title = "Foetal Weight (g)"))
    return(plot)
  })

}

shinyApp(ui = ui, server = server)
