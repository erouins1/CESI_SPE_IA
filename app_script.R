# Description: This script contains the code for the shiny app
# Import of libraries
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Foetal Weight Prediction"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        /* CSS to center the title of all boxes in the sidebar */
        .sidebar .box-header .box-title {
          text-align: center;
          display: block;
        }

        .form-group > label {
          color: black;  /* Force label color to black */
          visibility: visible;  /* Force labels to be visible */
          display: block;  /* Ensure labels are not set to display:none */
          text-align: center;  /* Center the labels */
        }
      "))
    ),
    box(
      width = "100%",
      title = "Input Parameters",
      sliderInput("MaternalAgeSlider", "Maternal Age (years)", min = 14, max = 60, value = 37),
      sliderInput("MaternalWeightSlider", "Maternal Weight (kg)", min = 40, max = 150, value = 95),
      sliderInput("MaternalHeightSlider", "Maternal Height (cm)", min = 120, max = 230, value = 175),
      selectInput("MaternalParity", "Maternal Parity", choices = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)),
      selectInput("ChildGender",  "Child Gender", choices = c("Male", "Female")),
      sliderInput("GestationalAgeSlider", "Gestational Age (days)", min = 120, max = 320, value = 220),
    )
  ),
  dashboardBody(

  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

}

# Run the application
shinyApp(ui = ui, server = server)
