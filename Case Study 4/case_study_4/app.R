library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(countrycode)
library(maps)
library(jsonlite)

data_cia <- fromJSON("data_cia.json")

world_map <- map_data("world")
world_map$ISO3 <- countrycode::countrycode(sourcevar = world_map$region, origin = "country.name", destination = "iso3c", nomatch = NA)

merged_data <- left_join(world_map, data_cia, by = "ISO3")

ui <- fluidPage(
  titlePanel("CIA World Factbook 2020"),
  p("Welcome to my shiny app, which allows you to visualize variables from the CIA factbook on the world map, generate descriptive statistics and statistical graphics"),
  
  tabsetPanel(
    tabPanel("Univariate analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("univariate", "Select a variable:", 
                             choices = c("Median Age" = "median_age", 
                                         "Youth Unemployment Rate" = "youth_unempl_rate", 
                                         "Net Migration Rate" = "net_migr_rate", 
                                         "Population Growth Rate" = "pop_growth_rate", 
                                         "Electricity from Fossil Fuels" = "electricity_fossil_fuel", 
                                         "Life Expectancy" = "life_expectancy")),
                 actionButton("view_data", "View Raw Data"),
                 dataTableOutput("raw_data")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map", plotlyOutput("map_plot")),
                   tabPanel("Boxplot (overall)", plotlyOutput("boxplot_overall")),
                   tabPanel("Boxplot per continent", plotlyOutput("boxplot_continent"))
                 )
               )
             )
    ),
    tabPanel("Multivariate analysis"
             
    )
  )
)

server <- function(input, output, session) {
  
  selected_data <- reactive({
    data_cia %>% select(country, continent, input$univariate)
  })
  
  # Raw data output
  output$raw_data <- renderDataTable({
    req(input$view_data)
    head(selected_data(), 15)
  })
  
  output$map_plot <- renderPlotly({
    req(input$univariate)
    p <- ggplot(merged_data, aes(x = long, y = lat, group = group, fill = get(input$univariate), text = country)) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c() +
      labs(fill = input$univariate) +
      theme_void()
    
    ggplotly(p, tooltip = c("text", input$univariate))
  })
  
  
  
}

shinyApp(ui = ui, server = server)