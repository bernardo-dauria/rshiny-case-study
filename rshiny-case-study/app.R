library(shiny)
library(gapminder)

dataPanel <- tabPanel("Data",
                      tableOutput("data")
)

# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 dataPanel
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  output$data <- renderTable(gapminder)
}

# Run the application 
shinyApp(ui = ui, server = server)
