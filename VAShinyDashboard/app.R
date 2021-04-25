
library(shiny)
library(tidyverse)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyquant)
library(plotly)
library(reactable)
library(shinybusy)
library(lubridate)
library(xts)
library(shinydashboard)
library(shinydashboardPlus)

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(
        title = tagList(
            tags$span(
                class = "logo-mini", "SSA"
            ),
            tags$span(
                class = "logo-lg", "Simple Stocks Analyzer"
            )
        ),
        titleWidth = 250,
        tags$li(class = "dropdown", 
                tags$a(href = "https://isss608stock.netlify.app/#guide", 
                       target="_blank",
                       tags$i(class = "fas fa-question-circle"),
                       tags$span("Help"))
                )
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Explore", tabName = "explore", icon = icon("chart-bar")),
            menuItem(text = "Analysis", icon = icon("chart-line"),
                     menuSubItem("Technical Analysis", tabName = "ta"),
                     menuSubItem("Portfolio Analysis", tabName = "portfolio")
            ),
            menuItem("Forecast", tabName = "forecast", icon = icon("dice-d20"))
        )
    ),
    dashboardBody(
        # shinyDashboardThemes(
        #     theme = "grey_light"
        # ),
        
        # tags$head(tags$style(HTML('
        # /* logo */
        # .skin-blue .main-header .logo {
        #                       background-color: #56cc9d;
        #                       }
        # 
        # /* logo when hovered */
        # .skin-blue .main-header .logo:hover {
        #                       background-color: #78c2ad;
        #                       }
        # 
        # /* navbar (rest of the header) */
        # .skin-blue .main-header .navbar {
        #                       background-color: #56cc9d;
        #                       }
        # /* toggle button when hovered  */                    
        # .skin-blue .main-header .navbar .sidebar-toggle:hover{
        #                       background-color: #78c2ad;
        #                       }'))),
        
        tags$head(tags$style(HTML('
          .nav-tabs-custom {
            margin-bottom: 0px;
            box-shadow: none;
          }
        '))),
            
        tabItems(
            # Explore tab content
            tabItem(tabName = "explore",
                    exploreUI("explore")
            ),
            
            # TA tab content
            tabItem(tabName = "ta",
                    TAUI("TA")
            ),
            
            # Portfolio tab content
            tabItem(tabName = "portfolio",
                    portfolioUI("portfolio")
            ),
            
            # Forecast tab content
            tabItem(tabName = "forecast",
                    forecastUI("forecast")
            )
        )
    )
)

server <- function(input, output) {
    exploreServer("explore")
    TAServer("TA")
    portfolioServer("portfolio")
    forecastServer("forecast")
}

shinyApp(ui, server)