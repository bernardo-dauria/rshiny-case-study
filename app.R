library(shiny)
library(tidyverse)
library(magrittr)
library(gapminder)
library(shinythemes)
library(shinyjs)
library(leaflet)

# taken from https://simplemaps.com/data/es-cities
esDB <- read_csv("es.csv") %>% drop_na() %>% mutate(popReduced = log(population/1000))
maxPop <- esDB$population %>% max(na.rm=TRUE)
#cat(file=stderr(), "maxPop:", maxPop, "\n")

gapminder %<>% mutate_at(c("year", "country"), as.factor)
gapminder_years = levels(gapminder$year) %>% str_sort()
gapminder_countries = levels(gapminder$country)


myHeader <- div(id="advanced",
  useShinyjs(),
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
  ),
  downloadButton("report", "Generate report")
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
                     leafletOutput('map', width = '100%', height = '300px'),
                     absolutePanel(top = 10, right = 10, id = 'controls',
                                   sliderInput('sizePop', 'Polulation Size', 0, maxPop, maxPop/10)
                                   ),
                     DT::DTOutput("esTable")
            )

# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
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
    
    esDBfiltered <- reactive(esDB %>% filter (population <= input$sizePop))
    
    output$map <- renderLeaflet({
        esDBfiltered() %>%
        leaflet() %>%
        addTiles() %>%
        setView( 0, 39.82, zoom = 5) %>%
        addCircleMarkers(
          popup = ~ city,
          radius = ~ popReduced,
          fillColor = 'red', color = 'red', weight = 1
        )
    })
    
    output$esTable <- DT::renderDT(esDBfiltered())
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          selYear = isolate(input$selYear),
          selCountry = isolate(input$selCountry)
        )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
