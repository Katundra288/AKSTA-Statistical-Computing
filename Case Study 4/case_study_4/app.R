library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(countrycode)
library(maps)
library(jsonlite)
library(shinyjs)
library('rsconnect')

# Link to the app on shinyapps.io
# https://akstacasestudy.shinyapps.io/case_study_4/


data_cia <- fromJSON("data_cia.json")

world_map <- map_data("world")
world_map$ISO3 <- countrycode::countrycode(sourcevar = world_map$region, origin = "country.name", destination = "iso3c", nomatch = NA)

merged_data <- left_join(world_map, data_cia, by = "ISO3")

variable_names <- list(
  "median_age" = "Median Age",
  "youth_unempl_rate" = "Youth Unemployment Rate",
  "net_migr_rate" = "Net Migration Rate",
  "pop_growth_rate" = "Population Growth Rate",
  "electricity_fossil_fuel" = "Electricity (Fossil Fuel)",
  "life_expectancy" = "Life Expectancy"
)

ui <- fluidPage(
  useShinyjs(),
  includeCSS("styles.css"),
  
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
                                         "Electricity (Fossil Fuels)" = "electricity_fossil_fuel", 
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
    tabPanel("Multivariate analysis" ,
             sidebarLayout(
               sidebarPanel(
                 selectInput("multivariate_var1", "Select variable 1:",
                             choices = c("Median Age" = "median_age", 
                                         "Youth Unemployment Rate" = "youth_unempl_rate", 
                                         "Net Migration Rate" = "net_migr_rate", 
                                         "Population Growth Rate" = "pop_growth_rate", 
                                         "Electricity (Fossil Fuels)" = "electricity_fossil_fuel", 
                                         "Life Expectancy" = "life_expectancy")),
                 selectInput("multivariate_var2", "Select variable 2:",
                             choices = c("Median Age" = "median_age", 
                                         "Youth Unemployment Rate" = "youth_unempl_rate", 
                                         "Net Migration Rate" = "net_migr_rate", 
                                         "Population Growth Rate" = "pop_growth_rate", 
                                         "Electricity (Fossil Fuels)" = "electricity_fossil_fuel", 
                                         "Life Expectancy" = "life_expectancy")),
                 selectInput("multivariate_scale", "Scale points by:",
                             choices = c("Area" = "area",
                                         "Population" = "population"))
                   
                ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Scatterplot", plotlyOutput("scatterplot") #if you want we can also delete the "Scatterplot" so it is 100% the task pic
                   
                 )
                 
               )
               
             )
             
            )
    ) #tabpanel
  ) #tabsetpanel
) #ui

server <- function(input, output, session) {
  
  selected_data <- reactive({
    data_cia %>% select(country, continent, input$univariate)
  })
  
  # Raw data output
  output$raw_data <- renderDataTable({
    req(input$view_data)
    df <- selected_data()
    colnames(df)[3] <- variable_names[[input$univariate]]
    head(df, 15)
  })
  
  output$boxplot_overall <- renderPlotly({
    req(input$univariate)
    data_filtered <- data_cia %>% filter(!is.na(continent))
    p <- ggplot(data_filtered, aes(x = "", y = get(input$univariate))) +
      geom_boxplot() +
      labs(y = input$univariate, x = "") +
      theme_minimal() +
      theme_grey()
    
    ggplotly(p)
  })
  
  output$boxplot_continent <- renderPlotly({
    req(input$univariate)
    data_filtered <- data_cia %>% filter(!is.na(continent))
    p <- ggplot(data_filtered, aes(x = continent, y = get(input$univariate), fill = continent)) +
      geom_boxplot() +
      labs(y = input$univariate, x = "Continent") +
      theme_minimal() +
      theme_grey() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  

  
  output$map_plot <- renderPlotly({
    req(input$univariate)
    p <- ggplot(merged_data, aes(x = long, y = lat, group = group, fill = get(input$univariate), text = country)) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c() +
      labs(fill = variable_names[[input$univariate]]) +
      theme_void()
    
    ggplotly(p, tooltip = c("text", input$univariate))
  })
  
  output$scatterplot <- renderPlotly({
    req(input$multivariate_var1)
    req(input$multivariate_var2)
    req(input$multivariate_scale)
    data_filtered <- data_cia %>% filter(!is.na(continent))
    sp <- ggplot(data_filtered, aes_string( 
                                   x = input$multivariate_var1, 
                                   y = input$multivariate_var2, 
                                   size = input$multivariate_scale,
                                   text = "country",
                                   color = "continent" )) +
      geom_point(alpha=0.7) +
      labs(
        title = paste("Scatterplot of", input$multivariate_var1, "and", input$multivariate_var2, "scaled by", input$multivariate_scale), #hope the title is fine?
        x = input$multivariate_var1,
        y = input$multivariate_var2,
        color = "continent"
      ) +
      theme_minimal()+
      theme_grey()
    ggplotly(sp, tooltip = c(input$multivariate_var1, input$multivariate_var2, input$multivariate_scale, "text")) #strangely the order is different in the app
  }) 
  
  
  
}

shinyApp(ui = ui, server = server)