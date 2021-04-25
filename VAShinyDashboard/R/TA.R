library(shiny)
library(tidyverse)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyquant)

tickers <- c("AAPL","MSFT","BAC","JPM","AAL","SAVE","JNJ","PFE")

prices <- tq_get(tickers, 
                 get  = "stock.prices",
                 from = "2011-01-01",
                 to   = today(),
                 complete_cases = F) %>% 
  select(symbol,date,open,close,high,low,volume)

TAUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Application title
    #fluidRow (column(6,offset =4, titlePanel(h3("Technical Analysis")))),
    
    fluidRow(
      box(title = "Parameters",
          status = "primary",
          width = 3,
          dateRangeInput(ns("dates"),
                         "Date range",
                         start = Sys.Date() - 100,
                         end = Sys.Date()),
          
          selectInput(
            inputId = ns("Chart"), 
            label = "Single Stock Analysis- Charts ",
            choices = c(
              "Basic - Closing Price" = "prices",
              "Basic - Closing Volume" = "volume",
              "Returns - Monthly" = "returns",
              "Returns - Yearly Returns" = "returns_yearly",
              "--------------------------------------------" = "none",
              "Candlestick" = "candlestick",
              "Averages - Simple Moving Average" = "sma",
              "Averages - Exponential Moving Average" = "ema",
              "Averages - Double Exponential Moving Average" = "dema",
              "Averages - Elastic volume-weighted moving averages" = "evwma",
              "Simple Moving Average +Bollinger Bands" = "bollinger"
            )),
          
          radioButtons(
            inputId = ns("Stocks"),
            label   = "Select one stock:",
            choices = c(
              "Apple" = tickers[1], 
              "Microsoft"= tickers[2],
              "Bank of America" = tickers[3],
              "Jp Morgan" = tickers[4],
              "American Airlines Group Inc" = tickers[5],
              "Spirit Airlines Incorporated" = tickers[6],
              "Johnson & Johnson" = tickers[7],
              "Pfzier"= tickers[8]))
          
      ),       
      
      box(title = "Technical Analysis", status = "success", solidHeader = TRUE,width = 9,
          plotOutput(ns("plot"),
                     height = "600px")
      )
      
    )
  )
}

TAServer <- function(id, data, left, right) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot<-renderPlot({
        
        if(input$Chart == "bollinger"){
          prices %>%
            filter(symbol == input$Stocks) %>%
            filter(date>= (input$dates[2]-weeks(30))) %>%
            filter(date<= input$dates[2]) %>%
            ggplot(aes(x = date, y = close, open = open,
                       high = high, low = low, close = close)) +
            geom_candlestick(aes(open = open, high = high, low = low, close = close),
                             colour_up = "darkgreen", colour_down = "darkred",
                             fill_up  = "darkgreen", fill_down  = "darkred")+
            geom_bbands(ma_fun = SMA, sd = 2, n = 20,show.legend = TRUE) +
            labs (title = "Bollinger Bands with SMA Applied",
                  subtitle = "24 Weeks Candlestick Chart",
                  y = "Closing Price in USD", x = "")+
            scale_x_date(breaks = "2 weeks", date_labels =  "%Y %m %d") +
            coord_x_date(xlim = c(input$dates[2] - weeks(24), input$dates[2] ))+
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic")
              
            )
        }
        
        else if(input$Chart == "candlestick"){        
          prices %>%
            filter(symbol == input$Stocks) %>%
            filter(date>=input$dates[1]) %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(x = date, y =close)) +
            geom_candlestick(aes(open = open, high = high, low = low, close = close),
                             colour_up = "darkgreen", colour_down = "darkred", 
                             fill_up  = "darkgreen", fill_down  = "darkred")+
            labs(title = "Opening and Closing Prices",subtitle = "Candlestick Chart", y ="Price in USD", x = "Date in Month/Year") +
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            scale_color_tq(theme = "dark")+
            theme(plot.background = element_rect(fill = "white"),
                  panel.background = element_rect(fill="light grey"),
                  legend.text = element_text(colour="black"))+
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        else  if (input$Chart == "volume"){
          prices %>%
            filter(symbol == input$Stocks) %>%
            filter(date>=input$dates[1]) %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(x = date, y = volume)) +
            geom_segment(aes(xend = date, yend = 0, color = volume,size = 0.2)) + 
            geom_smooth(method = "loess", se = FALSE) +
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            labs(title = "Volume Chart", 
                 subtitle = "Charting Daily Volume", 
                 y = "Volume", x = "") +
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 
          
        }
        
        else  if (input$Chart == "evwma"){
          prices %>%
            filter(symbol == input$Stocks) %>%
            filter(date>=input$dates[2]-250) %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(x = date, y = close, volume = volume )) +
            geom_candlestick(aes(open = open, high = high, low = low, close = close),
                             colour_up = "darkgreen", colour_down = "darkred", 
                             fill_up  = "darkgreen", fill_down  = "darkred")+
            geom_ma(ma_fun = EVWMA, n = 20, wilder = TRUE, color  = "blue", size = 1.25) +
            geom_ma(ma_fun = EVWMA, n = 50, wilder = TRUE, color = "red", size = 1.25) + 
            labs(title = "Elastic, volume-weighted moving averages", 
                 subtitle = "Bar Chart-20 (Blue) and 50-Day (Red) - Plotted for 250 days from end date", 
                 y = "Closing Price", x = "") +
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            coord_x_date(xlim = c(input$dates[2] - 168,input$dates[2]))
          
        }   
        else  if (input$Chart == "sma"){
          prices %>%
            filter(symbol == input$Stocks) %>%
            filter(date>=input$dates[2]-250) %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(x = date, y = close)) +
            geom_candlestick(aes(open = open, high = high, low = low, close = close),
                             colour_up = "darkgreen", colour_down = "darkred", 
                             fill_up  = "darkgreen", fill_down  = "darkred")+
            geom_ma(ma_fun = SMA, n = 20, wilder = TRUE, color  = "blue", size = 1.25) +
            geom_ma(ma_fun = SMA, n = 50, wilder = TRUE, color = "red", size = 1.25) + 
            labs(title = "Simple Moving Average Chart", 
                 subtitle = "Bar Chart-20 (Blue) and 50-Day (Red) - Plotted for 250 days from end date", 
                 y = "Closing Price", x = "") +
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            coord_x_date(xlim = c(input$dates[2] - 168,input$dates[2]))
          
        }   
        else  if (input$Chart == "dema"){
          prices %>%
            filter(symbol == input$Stocks) %>%
            filter(date>=input$dates[2]-250) %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(x = date, y = close)) +
            geom_candlestick(aes(open = open, high = high, low = low, close = close),
                             colour_up = "darkgreen", colour_down = "darkred", 
                             fill_up  = "darkgreen", fill_down  = "darkred")+
            geom_ma(ma_fun = DEMA, n = 20, wilder = TRUE, color  = "blue", size = 1) +
            geom_ma(ma_fun = DEMA, n = 50, wilder = TRUE, color = "red", size = 1) + 
            labs(title = "Double-exponential moving average", 
                 subtitle = "Bar Chart-20 (Blue) and 50-Day (Red) - Plotted for 250 days from end date", 
                 y = "Closing Price", x = "") +
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            coord_x_date(xlim = c(input$dates[2] - 168,input$dates[2]))
          
        }   
        
        else  if (input$Chart == "ema"){
          prices %>%
            filter(symbol == input$Stocks) %>%
            filter(date>=input$dates[2]-250) %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(x = date, y = close)) +
            geom_candlestick(aes(open = open, high = high, low = low, close = close),
                             colour_up = "darkgreen", colour_down = "darkred", 
                             fill_up  = "darkgreen", fill_down  = "darkred")+
            geom_line()+
            geom_point(color="red")+
            geom_ma(aes(volume=volume),ma_fun = EMA, n = 20, wilder = TRUE, color  = "blue", size = 1) +
            geom_ma(aes(volume=volume),ma_fun = EMA, n = 50, wilder = TRUE, color = "red", size = 1) + 
            labs(title = "Exponetial Moving Average Chart", 
                 subtitle = "Bar Chart-20 (Blue) and 50-Day (Red) - Plotted for 250 days from end date", 
                 y = "Closing Price", x = "") +
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            coord_x_date(xlim = c(input$dates[2] - 168,input$dates[2]))
          
        }     
        else  if (input$Chart == "returns"){
          
          tq_get(c(input$Stocks), get="stock.prices") %>%
            tq_transmute(select=adjusted,
                         mutate_fun=periodReturn,
                         period="monthly",
                         col_rename = "monthly_return") %>%
            filter(date>=input$dates[1]) %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(date, monthly_return)) +
            labs(title = "Monthly Return",subtitle = "Line Chart") + 
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            geom_bar(stat = "identity", fill = palette_light()[[1]])+
            geom_smooth(method = "lm")
          
        }
        else if (input$Chart == "prices"){
          prices%>%
            filter(symbol == input$Stocks) %>%
            filter(date>=input$dates[1]) %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(x = date, y = close)) +
            geom_line() +
            labs(title = "Prices", subtitle= "Simple Line Chart", y="Closing Price in USD", x = "Date in Month/Year", size =1.25 ) +
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            scale_color_tq(theme = "dark") +
            scale_y_continuous(labels = scales::dollar)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        else  if (input$Chart == "returns_yearly"){
          
          
          tq_get(c(input$Stocks), get="stock.prices") %>%
            tq_transmute(select=adjusted,
                         mutate_fun=periodReturn,
                         period="yearly",
                         col_rename = "yearly_return") %>%
            filter(date>= "2001-01-01") %>%
            filter(date<=input$dates[2]) %>%
            ggplot(aes(date, yearly_return)) +
            labs(title = "Yearly Return",subtitle = "Line Chart") + 
            scale_x_date(date_breaks = "1 year", date_labels =  "%b %Y") +
            theme(
              plot.title = element_text(color = "black", size = 16, face = "bold"),
              plot.subtitle = element_text(color = "black",size = 14),
              plot.caption = element_text(color = "green", face = "italic"))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            geom_line()+
            geom_point(col="red")
        }    
        
        
      })
      
    }
  )
}
