library(shiny)
library(tidyverse)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyquant)
library(reactable)
library(shinybusy)
library(parsnip)
library(glmnet)
library(randomForest)
library(kernlab)


forecastUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 2,
             
             accordion(
               id = "parameters",
               width = NULL,
               accordionItem(
                 title = "Data Preprocessing",
                 status = "success",
                 collapsed = FALSE,
                 # selectInput(ns("stock"), "Stock", 
                 #             choices = list("Apple" = "AAPL", "Google" = 'GOOG',
                 #                            "Tesla" = "TSLA"), selected = "AAPL"),
                 selectizeInput(
                   ns("stock"),
                   "Stock",
                   choices = c("AAPL", "MSFT", "BAC", "JPM", "AAL", "SAVE",
                               "JNJ", "PFE"),
                   multiple = FALSE,
                   selected = c("AAPL"),
                   options = list(maxItems = 1, create = TRUE)
                 ),
                 dateRangeInput(ns("dates"), 
                                "Date range",
                                start = Sys.Date() - 100, 
                                end = Sys.Date()),
                 sliderInput(ns("validation"), 
                             "Training - Validation Split",
                             min = 0.6,
                             max = 0.9,
                             value = 0.8,
                             step = 0.05),
                 numericInput(ns("horizon"), 
                              "Forecast Horizon (Days)", 
                              value = 10),
                 radioButtons(ns("date_features"), "Date Features:",
                              c("Date Derivatives" = "derivatives",
                                "Fourier Transformation" = "fourier",
                                "Both" = "both"),
                              selected = "both"),
                 sliderInput(ns("differencing"), "Differencing Lag:",
                             min = 0, max = 5,
                             value = 1),
                 actionButton(ns("fit"), 
                              "Train and Forecast",
                              width = "100%",
                              class = "btn-danger"),
                 textOutput(ns("missing_params"))
               ),
               accordionItem(
                 title = "Parameters Tuning",
                 status = "primary",
                 collapsed = TRUE,
                 selectInput(ns("model"), "Model", 
                             choices = list("ARIMA" = "arima",
                                            "Prophet" = "prophet",
                                            "ElasticNet" = "glm",
                                            "Random Forest" = "rf",
                                            "XGBoost" = "xgb",
                                            "SVM RBF" = "svm",
                                            "ARIMA Boosted" = "arima_b",
                                            "Prophet Boosted" = "prophet_b"),
                             selected = "arima"),
                 conditionalPanel(
                   condition = "input.model == 'arima'",
                   sliderInput(ns("ar1"), "Order of auto-regressive (AR):",
                               min = 0, max = 5,
                               value = 2),
                   sliderInput(ns("ar2"), "Order of integration for differencing:",
                               min = 0, max = 2,
                               value = 1),
                   sliderInput(ns("ar3"), "Order of moving average (MA):",
                               min = 0, max = 5,
                               value = 2),
                   ns = ns
                 ),
                 conditionalPanel(
                   condition = "input.model == 'prophet'",
                   
                   sliderInput(ns("prophet1"), 
                               "Number of potential changepoints",
                               min = 1,
                               max = 100,
                               value = 25,
                               step = 1),
                   
                   sliderInput(ns("prophet2"), 
                               "Changepoints range",
                               min = 0.1,
                               max = 1,
                               step = 0.1,
                               value = 0.8),
                   ns = ns
                 ),
                 conditionalPanel(
                   condition = "input.model == 'glm'",
                   sliderInput(ns("glm1"), 
                               "Penalty",
                               min = 0.01,
                               max = 0.1,
                               value = 0.01,
                               step = 0.01),
                   
                   sliderInput(ns("glm2"), 
                               "Mixture",
                               min = 0.1,
                               max = 1,
                               step = 0.1,
                               value = 0.5),
                   ns = ns
                 ),
                 conditionalPanel(
                   condition = "input.model == 'rf'",
                   
                   numericInput(ns("rf1"), 
                                "Number of trees",
                                value = 500),
                   
                   sliderInput(ns("rf2"), 
                               "Minimum data points",
                               min = 1,
                               max = 10,
                               step = 1,
                               value = 1),
                   ns = ns
                 ),
                 conditionalPanel(
                   condition = "input.model == 'xgb'",
                   
                   numericInput(ns("xgb1"), 
                                "Number of trees",
                                value = 500),
                   
                   sliderInput(ns("xgb2"), 
                               "Minimum data points",
                               min = 1,
                               max = 10,
                               step = 1,
                               value = 1),
                   
                   sliderInput(ns("xgb3"), 
                               "Tree depth",
                               min = 5,
                               max = 30,
                               step = 1,
                               value = 15),
                   ns = ns
                 ),
                 conditionalPanel(
                   condition = "input.model == 'svm'",
                   
                   sliderInput(ns("svm1"), 
                               "Cost",
                               min = 0.1,
                               max = 1,
                               step = 0.1,
                               value = 1),
                   
                   sliderInput(ns("svm2"), 
                               "Margin",
                               min = 0.1,
                               max = 1,
                               step = 0.1,
                               value = 0.1),
                   ns = ns
                 ),
                 conditionalPanel(
                   condition = "input.model == 'arima_b'",
                   
                   sliderInput(ns("arb1"), "Order of auto-regressive (AR):",
                               min = 0, max = 5,
                               value = 2),
                   
                   sliderInput(ns("arb2"), "Order of integration for differencing:",
                               min = 0, max = 2,
                               value = 1),
                   
                   sliderInput(ns("arb3"), "Order of moving average (MA):",
                               min = 0, max = 5,
                               value = 2),
                   
                   numericInput(ns("arb4"), 
                                "Number of trees",
                                value = 500),
                   
                   sliderInput(ns("arb5"), 
                               "Minimum datapoints",
                               min = 1,
                               max = 10,
                               step = 1,
                               value = 1),
                   
                   sliderInput(ns("arb6"), 
                               "Tree depth",
                               min = 5,
                               max = 30,
                               step = 1,
                               value = 15),
                   ns = ns
                 ),
                 conditionalPanel(
                   condition = "input.model == 'prophet_b'",
                   
                   sliderInput(ns("prophet_b1"), 
                               "Number of potential changepoints",
                               min = 1,
                               max = 100,
                               value = 25,
                               step = 1),
                   
                   sliderInput(ns("prophet_b2"), 
                               "Changepoints range",
                               min = 0.1,
                               max = 1,
                               step = 0.1,
                               value = 0.8),
                   
                   numericInput(ns("prophet_b3"), 
                                "Number of trees",
                                value = 500),
                   
                   sliderInput(ns("prophet_b4"), 
                               "Minimum datapoints",
                               min = 1,
                               max = 10,
                               step = 1,
                               value = 1),
                   
                   sliderInput(ns("prophet_b5"), 
                               "Tree depth",
                               min = 5,
                               max = 30,
                               step = 1,
                               value = 15),
                   
                   ns = ns
                 )
               )
             )
             
      ),
      column(width = 5,
             box(
               title = "Invidual Models", 
               solidHeader = TRUE, 
               status = "maroon",
               width = NULL,
               tabBox(
                 id = "charts",
                 width = NULL,
                 tabPanel("Validation",
                          id = "train",
                          plotlyOutput(ns("forecast_plot"),
                                       width = "100%",
                                       height = "760px"),
                          class = "border-bottom-0"),
                 tabPanel("Forecast",
                          id = "forecast",
                          plotlyOutput(ns("forecast_refit_plot"),
                                       width = "100%",
                                       height = "760px"),
                          class = "border-bottom-0"),
                 tabPanel("Residuals",
                          id = "residual",
                          plotlyOutput(ns("residuals_plot"),
                                       width = "100%",
                                       height = "760px"),
                          class = "border-bottom-0")
               )
             )
      ),
      column(width = 5,
             box(
               title = "Multiple Models", 
               solidHeader = TRUE, 
               status = "teal",
               width = NULL,
               tabBox(
                 id = "charts2",
                 width = NULL,
                 tabPanel("Validation",
                          id = "traincomb",
                          plotlyOutput(ns("forecast_plot_multi"),
                                       width = "100%",
                                       height = "360px")),
                 tabPanel("Forecast",
                          id = "forecastcomb",
                          plotlyOutput(ns("forecast_refit_plots_multi"),
                                       width = "100%",
                                       height = "360px")),
                 tabPanel("Residuals",
                          id = "residualcomb",
                          plotlyOutput(ns("residuals_plots_multi"),
                                       width = "100%",
                                       height = "360px"))
               )
             ),
             uiOutput(ns("acc_table_location"))
      )
      
    )
    
  )
}

forecastServer <- function(id, data, left, right) {
  moduleServer(
    id,
    function(input, output, session) {
     
      observe({
        if (!isTruthy(input$horizon)) {
          showModal(modalDialog(
            title = "Missing Parameters",
            "Please fill in all parameters"
          ))
        }
      })

      observeEvent(input$fit, {
        validate(
          need(isTruthy(input$horizon), FALSE)
        )
        
        show_modal_spinner(
          spin = "fingerprint",
          color = "#FF8C00",
          text = "Please wait while we train the models..."
        )
        
        output$acc_table_location <- renderUI({
          tagList(
            box(
              title = "Models Performance", 
              solidHeader = TRUE, 
              status = "warning",
              width = NULL,
              reactableOutput(session$ns("acc_table"))
            )
          )
        })
        
        stock <- tq_get(input$stock, get = "stock.prices",
                        from = format(input$dates[1]),
                        to = format(input$dates[2]))
        
        if (is.na(stock)) {
          showModal(modalDialog(
            title = "Stock data cannot be found",
            "Stock data cannot be found",
            footer = list(
              modalButton("OK")
              )
          ))
        } else {
          stock_tbl <- stock %>%
            select(date, close) %>%
            set_names(c("date", "value")) 
          
          if (input$differencing > 0 ) {
            stock_tbl <- stock_tbl %>%
              mutate(value = diff_vec(value, lag = input$differencing)) %>%
              mutate(value = replace_na(value, 0))
          }
          
          days_diff <- as.numeric(difftime(input$dates[2], input$dates[1], units = "days"))
          
          splits <- stock_tbl %>%
            time_series_split(assess = sprintf("%s days", round(days_diff*(1-input$validation))), 
                              cumulative = TRUE)
          
          recipe_spec <- recipe(value ~ date, training(splits))
          
          if (input$date_features == "derivatives" || input$date_features == "both") {
            recipe_spec <- recipe_spec %>%
              step_timeseries_signature(date) %>%
              step_rm(contains("am.pm"), contains("hour"), contains("minute"),
                      contains("second"), contains("xts"), contains("half"),
                      contains(".iso")) %>%
              step_normalize(date_index.num)
          }
          
          if (input$date_features == "fourier" || input$date_features == "both") {
            recipe_spec <- recipe_spec %>%
              step_fourier(date, period = 12, K = 1)
          }
          
          recipe_spec <- recipe_spec %>% 
            step_dummy(all_nominal())
          
          recipe_spec_parsnip <- recipe_spec %>%
            update_role(date, new_role = "ID")
          
          ##### ARIMA #####
          model_fit_arima <- arima_reg(
            non_seasonal_ar = input$ar1,
            non_seasonal_differences = input$ar2,
            non_seasonal_ma = input$ar3
          ) %>%
            set_engine("auto_arima") %>%
            fit(value ~ date, training(splits))
          ##### ARIMA #####
          
          ##### Prophet #####
          workflow_fit_prophet <- workflow() %>%
            add_model(
              prophet_reg(
                changepoint_num = input$prophet1,
                changepoint_range = input$prophet2
              ) %>% set_engine("prophet")
            ) %>%
            add_recipe(recipe_spec) %>%
            fit(training(splits))
          ##### Prophet #####
          
          ##### ElasticNet #####
          model_spec_glmnet <- linear_reg(
            penalty = input$glm1, 
            mixture = input$glm2
          ) %>%
            set_engine("glmnet")
          
          workflow_fit_glmnet <- workflow() %>%
            add_model(model_spec_glmnet) %>%
            add_recipe(recipe_spec_parsnip) %>%
            fit(training(splits))
          ##### ElasticNet #####
          
          ##### Random Forest #####
          model_spec_rf <- rand_forest(
            trees = input$rf1, min_n = input$rf2
          ) %>%
            set_engine("randomForest")
          
          workflow_fit_rf <- workflow() %>%
            add_model(model_spec_rf) %>%
            add_recipe(recipe_spec_parsnip) %>%
            fit(training(splits))
          ##### Random Forest #####
          
          ##### XGBoost #####
          workflow_fit_xgboost <- workflow() %>%
            add_model(
              boost_tree(
                trees = input$xgb1,
                min_n = input$xgb2,
                tree_depth = input$xgb3
              ) %>% set_engine("xgboost")
            ) %>%
            add_recipe(recipe_spec_parsnip) %>%
            fit(training(splits))
          ##### XGBoost #####
          
          ##### SVM #####
          workflow_fit_svm <- workflow() %>%
            add_model(
              svm_rbf(
                cost = input$svm1,
                margin = input$svm2
              ) %>% 
                set_engine("kernlab") %>%
                set_mode("regression")
            ) %>%
            add_recipe(recipe_spec_parsnip) %>%
            fit(training(splits))
          ##### SVM #####
          
          ##### ARIMA Boosted #####
          workflow_fit_arima_boosted <- workflow() %>%
            add_model(
              arima_boost(
                non_seasonal_ar = input$arb1,
                non_seasonal_differences = input$arb2,
                non_seasonal_ma = input$arb3,
                trees = input$arb4,
                min_n = input$arb5,
                tree_depth = input$arb6
              ) %>%
                set_engine(engine = "auto_arima_xgboost")
            ) %>%
            add_recipe(recipe_spec) %>%
            fit(training(splits))
          ##### ARIMA Boosted #####
          
          ##### Prophet Boosted #####
          model_spec_prophet_boost <- prophet_boost(
            changepoint_num = input$prophet_b1,
            changepoint_range = input$prophet_b2,
            trees = input$prophet_b3,
            min_n = input$prophet_b4,
            tree_depth = input$prophet_b5) %>%
            set_engine("prophet_xgboost") 
          
          workflow_fit_prophet_boost <- workflow() %>%
            add_model(model_spec_prophet_boost) %>%
            add_recipe(recipe_spec) %>%
            fit(training(splits))
          ##### Prophet Boosted #####
          
          model_table <- modeltime_table(
            model_fit_arima,
            workflow_fit_prophet,
            workflow_fit_glmnet,
            workflow_fit_rf,
            workflow_fit_xgboost,
            workflow_fit_svm,
            workflow_fit_arima_boosted,
            workflow_fit_prophet_boost) %>%
            update_model_description(1, "ARIMA") %>%
            update_model_description(2, "Prophet") %>%
            update_model_description(3, "ElasticNet") %>%
            update_model_description(4, "Random Forest") %>%
            update_model_description(5, "XGBoost") %>%
            update_model_description(6, "SVM") %>%
            update_model_description(7, "ARIMA Boosted") %>%
            update_model_description(8, "Prophet Boosted")
          
          calibration_table <- model_table %>%
            modeltime_calibrate(testing(splits))
          
          forecast_table <- calibration_table %>%
            modeltime_forecast(actual_data = stock_tbl)
          
          # forecast_plot <- calibration_table %>%
          #   modeltime_forecast(actual_data = stock_tbl) %>%
          #   plot_modeltime_forecast(.interactive = TRUE) %>%
          #   layout(colorway = c('#f3cec9', '#e7a4b6', '#cd7eaf', '#a262a9', '#6f4d96', '#3d3b72', '#182844')) %>%
          #   layout(plot_bgcolor='rgb(254, 247, 234)') 
          
          forecast_plots <- forecast_table %>%
            filter(.model_desc != "ACTUAL") %>%
            plot_modeltime_forecast(.facet_vars = .model_desc, 
                                    .facet_ncol = 2, 
                                    .interactive = FALSE,
                                    .facet_scales = "fixed",
                                    .legend_show = FALSE) +
            geom_line(data = forecast_table %>% 
                        filter(.model_desc == "ACTUAL") %>%
                        select(.index, .value),
                      color = "#2c3e50",
                      size = 0.5) +
            labs(title = paste("Forecast Validation Plot for", input$stock), subtitle = "From and To") +
            scale_colour_manual(values = c("#e31a1c", "#18bc9c", "#ccbe93", "#a6cee3",
                                           "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f")) +
            theme(panel.spacing = unit(0.25, "lines"),
                  #       strip.text = element_text(colour = 'white'),
                  #       strip.background =element_rect(fill="#FF8C00")
            )
          
          forecast_plots_multi <- forecast_table %>%
            plot_modeltime_forecast(.interactive = FALSE,
                                    .legend_show = TRUE,
                                    .legend_max_width = 25,
                                    .conf_interval_show = FALSE) +
            labs(title = paste("Forecast Validation Plot for", input$stock), subtitle = "From and To")
          
          forecast_accuracy_table <- calibration_table %>%
            modeltime_accuracy() %>%
            select(.model_desc, mae, mape, mase, rmse) %>%
            reactable(
              searchable = FALSE,
              filterable = FALSE,
              showPageSizeOptions = FALSE,
              striped = TRUE,
              highlight = TRUE,
              columns = list(
                #.model_id = colDef(name = "ID"),
                .model_desc = colDef(name = "Model"),
                mae = colDef(format = colFormat(digits = 2)),
                mape = colDef(format = colFormat(digits = 2)),
                mase = colDef(format = colFormat(digits = 2)),
                #smape = colDef(format = colFormat(digits = 2)),
                rmse = colDef(format = colFormat(digits = 2))#,
                #rsq = colDef(format = colFormat(digits = 2))
              )
            )
          
          forecast_refit_table <- calibration_table %>%
            modeltime_refit(stock_tbl) %>%
            modeltime_forecast(h = sprintf("%s days", input$horizon), actual_data = stock_tbl)
          
          
          # forecast_plot_refit <- calibration_table %>%
          #   modeltime_refit(stock_tbl) %>%
          #   modeltime_forecast(h = sprintf("%s days", input$horizon), actual_data = stock_tbl) %>%
          #   plot_modeltime_forecast(.interactive = TRUE) %>%
          #   layout(colorway = c('#f3cec9', '#e7a4b6', '#cd7eaf', '#a262a9', '#6f4d96', '#3d3b72', '#182844')) %>%
          #   layout(plot_bgcolor='rgb(254, 247, 234)') %>% 
          #   layout(paper_bgcolor='rgb(254, 247, 234)')
          
          
          forecast_refit_plots <- forecast_refit_table %>%
            filter(.model_desc != "ACTUAL") %>%
            plot_modeltime_forecast(.facet_vars = .model_desc, 
                                    .facet_ncol = 2, 
                                    .interactive = FALSE,
                                    .facet_scales = "fixed",
                                    .legend_show = FALSE) +
            geom_line(data = forecast_table %>% 
                        filter(.model_desc == "ACTUAL") %>%
                        select(.index, .value),
                      color = "#2c3e50",
                      size = 0.5) +
            labs(title = paste("Forecast Plot for", input$stock), subtitle = "From and To") +
            scale_colour_manual(values = c("#e31a1c", "#18bc9c", "#ccbe93", "#a6cee3",
                                           "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f")) +
            theme(panel.spacing = unit(0.25, "lines"))
          
          forecast_refit_plots_multi <- forecast_refit_table %>%
            plot_modeltime_forecast(.interactive = FALSE,
                                    .legend_show = TRUE,
                                    .legend_max_width = 25,
                                    .conf_interval_show = FALSE) +
            labs(title = paste("Forecast Plot for", input$stock), subtitle = "From and To")
          
          residuals_table <- calibration_table %>%
            modeltime_residuals()
          
          residuals_plots <- residuals_table %>%
            filter(.model_desc != "ACTUAL") %>%
            plot_modeltime_residuals(.type = "timeplot",
                                     .facet_vars = .model_desc, 
                                     .facet_ncol = 2, 
                                     .interactive = FALSE,
                                     .facet_scales = "fixed") +
            labs(title = paste("Residuals Plot for", input$stock), subtitle = "From and To") +
            scale_colour_manual(values = c("#e31a1c", "#18bc9c", "#ccbe93", "#a6cee3",
                                           "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f")) +
            theme(panel.spacing = unit(0.25, "lines"),
                  legend.position='none')
          
          residuals_plots_multi <- residuals_table %>%
            plot_modeltime_residuals(.type = "timeplot",
                                     .interactive = FALSE,
                                     .legend_max_width = 25) +
            labs(title = paste("Residuals Plot for", input$stock), subtitle = "From and To") +
            scale_colour_manual(values = c("#e31a1c", "#18bc9c", "#ccbe93", "#a6cee3",
                                           "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f"))
            
            
          
          output$forecast_plot <- renderPlotly({
            ggplotly(forecast_plots)
          })
          
          output$forecast_plot_multi <- renderPlotly({
            ggplotly(forecast_plots_multi)
          })
          
          output$forecast_refit_plot <- renderPlotly({
            ggplotly(forecast_refit_plots)
          })
          
          output$forecast_refit_plots_multi <- renderPlotly({
            ggplotly(forecast_refit_plots_multi)
          })
          
          output$residuals_plot <- renderPlotly({
            residuals_plots
          })
          
          output$residuals_plots_multi <- renderPlotly({
            residuals_plots_multi
          })
          
          output$acc_table <- renderReactable({
            forecast_accuracy_table
          })
        }
        
        remove_modal_spinner() # remove it when done
      })
    }
  )
}
