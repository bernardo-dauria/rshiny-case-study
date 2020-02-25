library(shiny)
library(tidyverse)
library(magrittr)
library(gapminder)

gapminder %<>% mutate_at("year", as.factor)
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
    ggplot(data=head(gapminder_year()), aes(x=country, y=pop, fill=year))
      + geom_bar(stat="identity", position=position_dodge())
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
