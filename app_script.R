# Description: This script contains the code for the shiny app
# Import of libraries
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    # Sidebar content
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    ),
    sliderInput("MaternalAgeSlider", "Maternal Age", min = 14, max = 60, value = 30),
    sliderInput("MaternalWeightSlider", "Maternal Weight", min = 40, max = 150, value = 70),
    sliderInput("MaternalHeightSlider", "Maternal Height", min = 120, max = 230, value = 170),
    selectInput("MaternalParity", "Maternal Parity", choices = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)),
    selectInput("ChildGender",  "Child Gender", choices = c("Male", "Female")),
    sliderInput("GestationalAgeSlider", "Gestational Age", min = 120, max = 320, value = 220),
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      box(
        title = "Controls",
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      )
    )
  )
)

# Define the prediction model and the data outputed
model <- function(input) {
  # Define the model
  model <- lm(mpg ~ wt, data = mtcars)

  # Predict the value
  prediction <- predict(model, newdata = data.frame(wt = input))

  return(prediction)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
