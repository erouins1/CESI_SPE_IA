# Description: This script contains the code for the shiny app
# Import of libraries
library(shiny)
library(bslib)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
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