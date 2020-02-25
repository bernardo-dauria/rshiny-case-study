library(shiny)
library(tidyverse)
library(gapminder)

gapminder_years = gapminder %>% select(year) %>% unique %>% arrange

dataPanel <- tabPanel("Data",
                      selectInput(
                        inputId = "selYear",
                        label = "Select the Year",
                        multiple = TRUE,
                        choices = gapminder_years,
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
  gapminder_year <- reactive({gapminder %>% filter(year %in% input$selYear)})
  output$data <- renderTable(gapminder_year());
  output$plot <- renderPlot(
    barplot(head(gapminder_year() %>% pull(pop)),
            main=paste("Population in",input$selYear),
            names.arg= head(gapminder_year() %>% pull(country))
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
