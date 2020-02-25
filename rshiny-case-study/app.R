library(shiny)
library(tidyverse)
library(gapminder)

dataPanel <- tabPanel("Data",
                      selectInput(
                        inputId = "selYear",
                        label = "Select the Year",
                        multiple = TRUE,
                        choices = gapminder %>% select(year) %>% unique %>% arrange
                        ),
                      tableOutput("data")
)

# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 dataPanel
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  output$data <- renderTable(gapminder %>% filter(year %in% input$selYear))
}

# Run the application 
shinyApp(ui = ui, server = server)
