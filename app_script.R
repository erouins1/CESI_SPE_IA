library(shiny)
library(shinydashboard)
library(plotly)  # Load plotly for interactive plots

predictFoetalWeight <- function(modelName, Maternal_age, Maternal_weight, Maternal_height, Parity, Sex, Gestational_age) {
  model <- readRDS(modelName)

  prediction <- numeric(Gestational_age)
  for (i in 1:Gestational_age) {
    data <- data.frame(
      Maternal_age = as.numeric(Maternal_age),
      Maternal_weight = as.numeric(Maternal_weight),
      Maternal_height = as.numeric(Maternal_height),
      Parity = as.numeric(Parity),
      Sex = as.factor(Sex),
      Gestational_age = i
    )
    prediction[i] <- predict(model, newdata = data)
  }
  return(prediction)
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
      selectInput("ChildGender",  "Child Gender", choices = c("Male", "Female")),
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
  output$plot <- renderPlotly({
    Maternal_age <- input$MaternalAgeSlider
    Maternal_weight <- input$MaternalWeightSlider
    Maternal_height <- input$MaternalHeightSlider
    Parity <- input$MaternalParity
    Sex <- input$ChildGender
    Gestational_age <- input$GestationalAgeSlider

    prediction <- predictFoetalWeight("./Models/polynomial_model.rds", Maternal_age, Maternal_weight, Maternal_height, Parity, Sex, Gestational_age)

    gest_ages <- 1:Gestational_age
    valid_indices <- prediction > 0
    filtered_predictions <- prediction[valid_indices]
    filtered_ages <- gest_ages[valid_indices]

    if (length(filtered_predictions) == 0) {
      return(NULL)
    }

    plot_ly(data = data.frame(Gestational_Age = filtered_ages, Foetal_Weight = filtered_predictions), x = ~Gestational_Age, y = ~Foetal_Weight, type = 'scatter', mode = 'lines+markers',
            text = ~paste("Gestational Age:", Gestational_Age, "days<br>Weight:", Foetal_Weight, "grams"), hoverinfo = "text") %>%
      layout(title = "Foetal Weight Prediction by Gestational Age",
             xaxis = list(title = "Gestational Age (days)"),
             yaxis = list(title = "Foetal Weight (grams)"))
  })
}

shinyApp(ui = ui, server = server)