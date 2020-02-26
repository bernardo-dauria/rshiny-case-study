library(shiny)
library(tidyverse)
library(magrittr)
library(gapminder)
library(shinythemes)
library(shinyjs)

esDB <- read_csv("es.csv")

gapminder %<>% mutate_at(c("year", "country"), as.factor)
gapminder_years = levels(gapminder$year) %>% str_sort()
gapminder_countries = levels(gapminder$country)


myHeader <- div(id="advanced",
  selectInput(
    inputId = "selYear",
    label = "Select the Year",
    multiple = TRUE,
    choices = gapminder_years,
    selected = c(gapminder_years[1])
  ),
  selectInput(
    inputId = "selCountry",
    label = "Select the Country",
    multiple = TRUE,
    choices = gapminder_countries,
    selected = c(gapminder_countries[1])
  )
)

dataPanel <- tabPanel("Data",
                      tableOutput("data")
)

plotPanel <- tabPanel("Plot",
                  fluidRow(
                    column(width = 8,
                      plotOutput("plot",
                                 hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                      )),
                    column(width = 4,
                           h2("Info"),
                           h3(
                             textOutput("hoverCountry", container = span),
                             textOutput("hoverYear", container = span),
                           ),
                           textOutput("hoverPop")
                    )
                  )
              )

plotlyPanel <- tabPanel("Plotly",
                        plotly::plotlyOutput("plotly")
)

mapPanel <- tabPanel("map",
                     DT::DTOutput("esTable")
            )

# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 useShinyjs(),
                 dataPanel,
                 plotPanel,
                 plotlyPanel,
                 mapPanel,
                 header = myHeader,
                 theme = shinytheme("united"),
                 id = "navBar"
)

# Define server logic required to draw a histogram
server <- function(input, output, session) { 
  
  gapminder_year <- reactive({gapminder %>%
      filter(year %in% input$selYear, country %in% input$selCountry)})
  
  output$data <- renderTable(gapminder_year());
  
  output$plot <- renderPlot(
    ggplot(gapminder_year(), aes(x=country, y=pop, fill=year))
    + geom_bar(stat="identity", position=position_dodge())
  )
  
  output$plotly <- plotly::renderPlotly(
    ggplot(gapminder_year(), aes(x=country, y=pop, fill=year))
    + geom_bar(stat="identity", position=position_dodge())
  )
  
  output$plot_hoverinfo <- renderPrint({
    cat("Hover (throttled):\n")
    str(input$plot_hover)
  })
  
  output$esTable <- DT::renderDT(head(esDB,30))
  
    hoverCountryIdx <- reactive({
      req(input$plot_hover$x)
      round(input$plot_hover$x)
      })
    hoverCountry <- reactive({
      req(hoverCountryIdx() > 0 & hoverCountryIdx() <= length(input$selCountry))
      input$selCountry[hoverCountryIdx()]
      })
    hoverYearIdx <- reactive(ceiling((input$plot_hover$x-hoverCountryIdx()+0.5)*length(input$selYear)))
    hoverYear <- reactive({
      req(input$plot_hover$x)
      req(hoverCountry() != "")
      input$selYear[hoverYearIdx()]}
      )
    output$hoverCountry <- renderText(hoverCountry())
    output$hoverYear <-renderText(hoverYear())
    output$hoverPop <-renderText(paste("Population: ",
                                       gapminder_year() %>% 
                                    filter(year == hoverYear(), country == hoverCountry()) %>%
                                    pull(pop)))
    
    observe({
      if (input$navBar == "map") {
        shinyjs::hide("advanced")
      } else {
        shinyjs::show("advanced")
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
