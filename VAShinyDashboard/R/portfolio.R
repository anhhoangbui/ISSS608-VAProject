library(shiny)
library(tidyverse)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyquant)

tickers <- c("AAPL", "MSFT", "BAC", "JPM", "AAL", "SAVE", "JNJ", "PFE")

prices <- tq_get(
  tickers,
  get  = "stock.prices",
  from = "2011-01-01",
  to   = today(),
  complete_cases = F
) %>%
  select(symbol, date, open, close, high, low, volume)

portfolioUI <- function(id) {
  ns <- NS(id)
  
  tagList(# Application title
    # fluidRow (column(6, offset = 4, titlePanel(
    #   h3("Portfolio Analysis")
    # ))),
    
    fluidRow (
      box(
        title = "Parameters",
        status = "primary",
        width = 3,

        dateRangeInput(
          ns("dates"),
          "Date range",
          start = Sys.Date() - 100,
          end = Sys.Date()
        ),
        
        selectInput(
          inputId = ns("chartportfolio"), 
          label = "Portfolio Analysis- Charts:",
          choices = c(
            "Closing Prices Line Chart" = "closingline",
            "Candlestick Chart -last 30 days" = "closingfacet",
            "Moving Average" = "ma",
            "Annual Returns" = "ar",
            "Portfolio Returns(All stocks weighted)" = "port_return",
            "Portfolio Growth(All stocks weighted)" = "port_growth")),
        
        
        fluidRow (
          column (8,selectInput(
            inputId = ns("Stock1"),
            label   = "Stock 1 of 4",
            choices = c("Apple" = tickers[1], 
                        "Microsoft"= tickers[2],
                        "Bank of America" = tickers[3],
                        "Jp Morgan" = tickers[4],
                        "American Airlines Group Inc" = tickers[5],
                        "Spirit Airlines Incorporated" = tickers[6],
                        "Johnson & Johnson" = tickers[7],
                        "Pfzier"= tickers[8]),
            selected = tickers[1])
          ),
          column (4, numericInput("s1wght", "Weightage", min = 0, max=1, value=0.0, step=0.05), style="display:inline-block")
          
        ),
        
        fluidRow (
          column ( 8, selectInput(
            inputId = ns("Stock2"),
            label   = "Stock 2 of 4",
            choices = c(
              "Apple" = tickers[1], 
              "Microsoft"= tickers[2],
              "Bank of America" = tickers[3],
              "Jp Morgan" = tickers[4],
              "American Airlines Group Inc" = tickers[5],
              "Spirit Airlines Incorporated" = tickers[6],
              "Johnson & Johnson" = tickers[7],
              "Pfzier"= tickers[8]),
            selected = tickers[2])
          ),
          column ( 4, numericInput("s2wght", "Weightage", min = 0, max=1, value=0.0, step=0.05),  style="display:inline-block")
          
        ),
        
        fluidRow (
          column ( 8, selectInput(
            inputId = ns("Stock3"),
            label   = "Stock 3 of 4",
            choices = c(
              "Apple" = tickers[1], 
              "Microsoft"= tickers[2],
              "Bank of America" = tickers[3],
              "Jp Morgan" = tickers[4],
              "American Airlines Group Inc" = tickers[5],
              "Spirit Airlines Incorporated" = tickers[6],
              "Johnson & Johnson" = tickers[7],
              "Pfzier"= tickers[8]),
            selected = tickers[3])
          ),
          column ( 4, numericInput("s3wght", "Weightage", min = 0, max=1, value=0.0, step=0.05),  style="display:inline-block")
          
        ),
        fluidRow (
          column ( 8, selectInput(
            inputId = ns("Stock4"),
            label   = "Stock 4 of 4",
            choices = c(
              "Apple" = tickers[1], 
              "Microsoft"= tickers[2],
              "Bank of America" = tickers[3],
              "Jp Morgan" = tickers[4],
              "American Airlines Group Inc" = tickers[5],
              "Spirit Airlines Incorporated" = tickers[6],
              "Johnson & Johnson" = tickers[7],
              "Pfzier"= tickers[8]),
            selected = tickers[4])),
          column ( 4, numericInput("s4wght", "Weightage", min = 0, max=1, value=0, step=0.05),  style="display:inline-block")
          
        )
        
        
        
      ),
      
      box(
        title = "Portfolio Analysis",
        status = "warning",
        solidHeader = TRUE,
        width = 9,
        plotOutput(ns("plot2"),
                   height = "600px")
      )
      
    ))
}

portfolioServer <- function(id, data, left, right) {
  moduleServer(id,
               function(input, output, session) {
                 output$plot2<-renderPlot(
                   {
                     
                     if    (input$chartportfolio == "closingline")
                       
                     {
                       tq_get(c(input$Stock1,input$Stock2,input$Stock3,input$Stock4),get="stock.prices") %>%
                         select(symbol,date,open,close,high,low,volume)%>%
                         filter(date>=input$dates[1]) %>%
                         filter(date<=input$dates[2]) %>%
                         ggplot(aes(date, close, color=symbol)) +
                         geom_line(size=1)+
                         labs(title = "Portfolio - Closing Prices- Line Chart",
                              subtitle = "All Stocks combined in a single chart",
                              y = "Closing Price in USD", x = "") +
                         scale_x_date(date_breaks = "1 week", date_labels =  "%b %Y") +
                         theme(
                           plot.title = element_text(color = "black", size = 16, face = "bold"),
                           plot.subtitle = element_text(color = "black",size = 14),
                           plot.caption = element_text(color = "green", face = "italic"))
                     }
                     else if (input$chartportfolio == "closingfacet")
                     {
                       tq_get(c(input$Stock1,input$Stock2,input$Stock3,input$Stock4),get="stock.prices") %>%
                         select(symbol,date,open,close,high,low,volume)%>%
                         filter(date >= ( TODAY() - days(30))) %>%
                         ggplot(aes(x = date, y = close, group=symbol)) +
                         geom_candlestick(aes(open = open, high = high, low = low, close = close),
                                          colour_up = "darkgreen", colour_down = "darkred",
                                          fill_up  = "darkgreen", fill_down  = "darkred")+
                         labs(title = "Portfolio - Open/Close - Candlestick Chart",
                              subtitle = "Facet - for all stocks",
                              y = "Closing Price", x = "") +
                         scale_x_date(date_breaks = "1 week", date_labels =  "%b %Y") +
                         facet_wrap(~symbol, ncol = 2, scale = "free_y") +
                         theme(
                           plot.title = element_text(color = "black", size = 16, face = "bold"),
                           plot.subtitle = element_text(color = "black",size = 14),
                           plot.caption = element_text(color = "green", face = "italic"))
                     }
                     else if (input$chartportfolio == "ma")
                     {
                       tq_get(c(input$Stock1,input$Stock2,input$Stock3,input$Stock4),get="stock.prices") %>%
                         select(symbol,date,open,close,high,low,volume)%>%
                         filter(date >= (input$dates[1]-250)) %>%
                         filter(date <= input$dates[2]) %>%
                         ggplot(aes(x = date, y = close, color = symbol)) +
                         geom_line(size = 1.5) +
                         geom_ma(n = 15, color = "darkblue", size = 1) + 
                         geom_ma(n = 50, color = "red", size = 1) +
                         labs(title = "Portfolio - Moving Average",subtitle = "Facet - for all stocks - 15 days ( darkblue), 50 days (red)",
                              x = "", y = "Closing Price in USD") +
                         facet_wrap(~ symbol, scales = "free_y") +
                         scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
                         scale_y_continuous(labels = scales::dollar)+
                         theme(
                           plot.title = element_text(color = "black", size = 16, face = "bold"),
                           plot.subtitle = element_text(color = "black",size = 14),
                           plot.caption = element_text(color = "green", face = "italic"))
                       
                     }
                     else if (input$chartportfolio == "port_return")
                     {
                       stock_returns_monthly <- c(input$Stock1,input$Stock2,input$Stock3,input$Stock4) %>%
                         tq_get(get  = "stock.prices",
                                from = input$dates[1],
                                to   = input$dates[2]) %>%
                         group_by(symbol) %>%
                         
                         tq_transmute(select     = adjusted, 
                                      mutate_fun = periodReturn, 
                                      period     = "monthly", 
                                      col_rename = "Ra")
                       wts <- c(input$s1wght,input$s2wght,input$s3wght,input$s4wght)
                       portfolio_returns_monthly <- stock_returns_monthly %>%
                         filter(date>=input$dates[1]) %>%
                         filter(date<=input$dates[2]) %>%
                         tq_portfolio(assets_col  = symbol, 
                                      returns_col = Ra, 
                                      weights     = wts, 
                                      col_rename  = "Ra")
                       portfolio_returns_monthly %>%
                         ggplot(aes(x = date, y = Ra)) +
                         geom_bar(stat = "identity", fill = palette_light()[[1]]) +
                         labs(title = "Portfolio Monthly Returns",
                              subtitle = "All Stocks with non-zero weights are included",
                              caption = "Shows an above-zero trend meaning positive returns",
                              x = "", y = "Monthly Returns") +
                         geom_smooth(method = "lm") +
                         theme(
                           plot.title = element_text(color = "black", size = 16, face = "bold"),
                           plot.subtitle = element_text(color = "black",size = 14),
                           plot.caption = element_text(color = "green", face = "italic"))+
                         scale_color_tq() +
                         scale_y_continuous(labels = scales::percent)+
                         scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
                     }
                     else if (input$chartportfolio == "ar")
                     {
                       Portfolio <- tq_get(c(input$Stock1,input$Stock2,input$Stock3,input$Stock4),get="stock.prices")
                       
                       PortfolioAnnual <- Portfolio %>%
                         group_by(symbol) %>%
                         tq_transmute(select     = adjusted, 
                                      mutate_fun = periodReturn, 
                                      period     = "yearly", 
                                      type       = "arithmetic")
                       
                       PortfolioAnnual %>%
                         ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
                         geom_col() +
                         geom_hline(yintercept = 0, color = palette_light()[[1]]) +
                         scale_y_continuous(labels = scales::percent) +
                         labs(title = "Annual Returns",
                              subtitle = "Annual Returns of all stocks since 2012",
                              y = "Annual Returns", x = "") + 
                         facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
                         theme(
                           plot.title = element_text(color = "black", size = 16, face = "bold"),
                           plot.subtitle = element_text(color = "black",size = 14),
                           plot.caption = element_text(color = "green", face = "italic"))+ 
                         scale_fill_tq()    
                     }
                     else if (input$chartportfolio == "port_growth")
                     {
                       wts <- c(input$s1wght,input$s2wght,input$s3wght,input$s4wght)
                       
                       stock_returns_monthly <- c(input$Stock1,input$Stock2,input$Stock3,input$Stock4) %>%
                         tq_get(get  = "stock.prices",
                                from = input$dates[1],
                                to   = input$dates[2]) %>%
                         group_by(symbol) %>%
                         tq_transmute(select     = adjusted, 
                                      mutate_fun = periodReturn, 
                                      period     = "monthly", 
                                      col_rename = "Ra")
                       
                       portfolio_growth_monthly <- stock_returns_monthly %>%
                         tq_portfolio(assets_col   = symbol, 
                                      returns_col  = Ra, 
                                      weights      = wts, 
                                      col_rename   = "investment.growth",
                                      wealth.index = TRUE) %>%
                         mutate(investment.growth = investment.growth * 10000)
                       
                       portfolio_growth_monthly %>%
                         ggplot(aes(x = date, y = investment.growth)) +
                         #            geom_line(position ="identity") +
                         labs(title = "Portfolio Growth for an initial investment of $10,000",
                              subtitle = "Stocks with non-zero weights are included",
                              x = "", y = "Portfolio Value") +
                         geom_smooth(method = "loess") +
                         theme(
                           plot.title = element_text(color = "black", size = 16, face = "bold"),
                           plot.subtitle = element_text(color = "black",size = 14),
                           plot.caption = element_text(color = "green", face = "italic"))+
                         theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                         scale_color_tq() +
                         scale_y_continuous(labels = scales::dollar)+
                         scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
                       
                     }
                     
                   })
               })
}
