library(shiny)
library(ggplot2)
library(tidyverse)
library(timetk)
library(plotly)
library(tidyr)
library(tidymodels)
library(tidyquant)
library(modeltime)
library(lubridate)
library(forecast)

tickers <- c("AAPL","MSFT","BAC","JPM","AAL","SAVE","JNJ","PFE")

prices <- tq_get(tickers, 
                 get  = "stock.prices",
                 from = "2011-01-01",
                 to   = today(),
                 complete_cases = F) %>% 
  select(symbol,date,open,close,high,low,volume)

exploreUI <- function(id) {
  ns <- NS(id)
  
  tagList(
          fluidRow(
            column(
              width = 2,
              box(
                title = "Parameters",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                
                selectizeInput(
                  ns("stocks"),
                  "Select Stocks",
                  choices = c("AAPL", "MSFT", "BAC", "JPM", "AAL", "SAVE",
                              "JNJ", "PFE"),
                  multiple = TRUE,
                  selected = c("AAPL", "MSFT"),
                  options = list(maxItems = 4, create = TRUE)
                ),
                
                # Input: Dates
                dateRangeInput(
                  ns("dates"),
                  "Date range",
                  start = Sys.Date() - 365,
                  end = Sys.Date()
                )
              )
            ),
            
            column(
              width = 5,
              box(
                title = "Price",
                closable = FALSE,
                width = NULL,
                #height = "400px",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "navy",
                sidebar = boxSidebar(
                  id = "priceparams",
                  width = 25,
                  selectInput(ns("price_at"), "Price at", 
                              choices = list("Adjusted" = 'adjusted',
                                             "Close" = "close", 
                                             "Open" = "open",
                                             "High" = "high",
                                             "Low" = "low"), 
                              selected = "adjusted"),
                  selectInput(ns("price_by"), "Price per", 
                              choices = list("Day" = "day", 
                                             "Week" = 'week',
                                             "Month" = "month"), 
                              selected = "day")
                ),
                plotlyOutput(
                  outputId = ns("prices"),
                  height = "500px"
                )
              ),
              
              box(
                title = "Auto Correlation",
                closable = FALSE,
                width = NULL,
                #height = "400px",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "orange",
                sidebar = boxSidebar(
                  id = "corrparams",
                  width = 25,
                  selectInput(ns("corr_for"), "Correlation for", 
                              choices = list("Adjusted" = 'adjusted',
                                             "Close" = "close", 
                                             "Open" = "open",
                                             "High" = "high",
                                             "Low" = "low"), 
                              selected = "adjusted"),
                  numericInput(ns("corr_lag"), 
                               "Lags (in days)", 
                               value = 30,
                               min = 1),
                  checkboxInput(ns("corr_noise"), 
                                "Show white noise bar", 
                                FALSE)
                ),
                plotlyOutput(outputId = ns("acf"),
                             height = "500px")
              )
            ),
            
            column(
              width = 5,
              box(
                title = "Volume",
                closable = FALSE,
                width = NULL,
                #height = "400px",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "success",
                sidebar = boxSidebar(
                  id = "volumeparam",
                  width = 25,
                  
                  selectInput(ns("volume_aggre"), "Aggregate by", 
                              choices = list("Sum" = "sum", 
                                             "Mean" = 'mean'), 
                              selected = "sum"),
                  selectInput(ns("volume_by"), "Volume per", 
                              choices = list("Day" = "day", 
                                             "Week" = 'week',
                                             "Month" = "month"), 
                              selected = "day")
                ),
                plotlyOutput(
                  outputId = ns("volumes"),
                  height = "500px"
                )
              ),
              
              box(
                title = "Anomaly",
                closable = FALSE,
                width = NULL,
                #height = "400px",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "purple",
                sidebar = boxSidebar(
                  id = "anomalyparams",
                  width = 25,
                  selectInput(ns("ano_for"), "Detect anomaly for", 
                              choices = list("Adjusted" = 'adjusted',
                                             "Close" = "close", 
                                             "Open" = "open",
                                             "High" = "high",
                                             "Low" = "low",
                                             "Volume" = "volume"), 
                              selected = "adjusted"),
                  selectInput(ns("ano_by"), "Anomaly per", 
                              choices = list("Day" = "day", 
                                             "Week" = 'week',
                                             "Month" = "month"), 
                              selected = "week")
                ),
                plotlyOutput(
                  outputId = ns("anomaly"),
                  height = "500px"
                )
              )
            )
          ))
}

exploreServer <- function(id, data, left, right) {
  moduleServer(
    id,
    function(input, output, session) {
      
      stock <- reactive(tq_get(input$stocks, get = "stock.prices", 
                      from = format(input$dates[1]), to  = format(input$dates[2])))
      
      #price_params <- reactiveValues(price_at = input$price_at, price_by = input$price_by)
      
      price_plots <- reactive({
        stock() %>%
          group_by(symbol) %>%
          summarise_by_time(
            date, 
            .by = input$price_by,
            price = mean(get(input$price_at)),
            symbol = symbol
          ) %>%
          plot_time_series(date, price,
                           .color_var = year(date),  
                           .interactive = FALSE,
                           .facet_ncol = 2,
                           .y_intercept = 0,
                           #.title = "Stock's Price",
                           #.x_lab = "Date",
                           #.y_lab = "Price (US$)",
                           .color_lab = "Year") +
          theme(axis.text.x = element_text(angle = 30, hjust = 1))
      })
      
      output$prices <- renderPlotly({
        ggplotly(price_plots())
      })
      
      volume_plots <- reactive({
        if (input$volume_aggre == 'sum') {
          stock() %>%
            group_by(symbol) %>%
            summarise_by_time(
              date, .by = input$volume_by,
              volume = SUM(volume),
              symbol = symbol
            ) %>%
            plot_time_series(date, volume, 
                             .interactive = F, 
                             .facet_ncol = 2,
                             .y_intercept = 0,                        
                             .title = "Transaction Volume",
                             .plotly_slider = FALSE) +
          theme(axis.text.x = element_text(angle = 30, hjust = 1))
        } else {
          stock() %>%
            summarise_by_time(
              date, .by = input$volume_by,
              volume = mean(volume),
              symbol = symbol
            ) %>%
            group_by(symbol) %>%
            plot_time_series(date, volume, 
                             .interactive = F, 
                             .facet_ncol = 2,
                             .y_intercept = 0,                        
                             .title = "Transaction Volume",
                             .plotly_slider = FALSE) +
          theme(axis.text.x = element_text(angle = 30, hjust = 1))
        }
        
      })
      
      output$volumes <- renderPlotly({
        ggplotly(volume_plots())
      })
      
      acf_plots <- reactive({
        stock() %>%
          group_by(symbol) %>%
          plot_acf_diagnostics(
            date, get(input$corr_for),
            .lags = sprintf("%s days", input$corr_lag),
            .facet_ncol = 2,
            .show_white_noise_bars = input$corr_noise,
            .interactive = TRUE
          )
      })
      
      output$acf <- renderPlotly({
        acf_plots()
      })

      anomaly_plots <- reactive({
        stock() %>%
          group_by(symbol) %>%
          summarise_by_time(
            date, .by = input$ano_by,
            value = mean(get(input$ano_for))
          ) %>%
          plot_anomaly_diagnostics(date, value,
                                   .message = FALSE,
                                   .facet_ncol = 2,
                                   .ribbon_alpha = 0.25,
                                   .interactive = FALSE,
                                   .legend_show = FALSE) +
          theme(axis.text.x = element_text(angle = 30, hjust = 1))
      })
      output$anomaly <- renderPlotly({
        ggplotly(anomaly_plots())
      })
    }
  )
}
