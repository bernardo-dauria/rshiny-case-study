library(shiny)
library(tidyverse)
library(gapminder)

dataPanel <- tabPanel("Data",
                      selectInput(
                        inputId = "selYear",
                        label = "Select the Year",
                        multiple = TRUE,
                        choices = gapminder %>% select(year) %>% unique %>% arrange,
                        selected = 0
                        ),
                      tableOutput("data")
)

plotPanel <- tabPanel("Plot",
                      plotOutput("plot")
                      )

# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 dataPanel,
                 plotPanel
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  output$data <- renderTable(gapminder %>% filter(year %in% input$selYear));
  output$plot <- renderPlot(
    barplot(head(gapminder %>% filter(year %in% input$selYear) %>% pull(pop)),
            main=paste("Population in",input$selYear), horiz=FALSE,
            names.arg= head(gapminder %>% filter(year %in% input$selYear) %>% pull(country))
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
