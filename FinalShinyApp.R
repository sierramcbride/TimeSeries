library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggeasy)
library(plotly)
library(fpp3)

# orig data 
Canadian_Gas <-canadian_gas %>%
  filter(year(Month)>= 1969 & year(Month)<= 2004)

# simple forecasting 
MODELNAMES <- c("Naive","SeasonalNaive", "Mean", "Drift")
js <- '.nav-tabs-custom .nav-tabs {
    background-color: #3c8dbc; color:white;}
    .nav-tabs-custom .nav-tabs li a{
      background-color: #3c8dbc; color:white; border-top-color: white;
    }
"'



# holt and holt winters 
MODELNAMES2 <- c("Holt","HoltWinters")

# arima 
MODELNAMES3 <- c("ManualArima","AutoArima")


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Time Series"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "M1"),
      menuItem("Full-Time Series", tabName = "M2"),
      menuItem("Other Plots", tabName = "M3"),
      menuItem("Simple Models", tabName = "M4"),
      menuItem("Exponential Smoothing", tabName = "M5"),
      menuItem("ARIMA", tabName = "M6")
    )
  ),
  dashboardBody(tabItems(
    # First tab 
    tabItem(tabName = "M1",
            includeMarkdown("FinalInstructions.Rmd")
    ),
    
    # Second tab 
    tabItem(tabName = "M2",
            fluidPage(
              plotlyOutput("plot1"),
              numericInput("Year1",
                           label = "Input Starting Year",
                           value = 1969),
              numericInput("Year2",
                           label = "Input Ending Year",
                           value = 2004),
              fluidRow(
                tags$style(js),
                tabBox(
                 
                  title = "Time Plot",
                  side= "right",height = "250px", width=6,
                  selected = "Description",
                  tabPanel("Interpretation", "Canadian gas production started in January of 1969 at 5.72 volumes, and it increased in a straight line until December of 
                  1973 where it topped out at 9.16 volumes. After that it flattens out to an average of 8.5 gallons until June of 1987. In 
                  January of 1988 there is a sharp increase in production to 12.66 gallons. Then there is another steady increase in production 
                  until December of 2000 where it tops out at 19.44 gallons. After that the trend flattens out again with the average being around 
                  18.40 gallons until it ends in December of 2004."),
                  tabPanel("Description", "A time plot shows all of the information collected on a subject over a certain period of time. 
               Each point on the graph represents a number in units that corresponds to a time the data was collected. 
               These points are then connected by a continuous line. By viewing this plot, you can view the trend pattern of the data. 
               A trend pattern shows a long-term increase or decrease in the data.")
                )
              )
            )
    ),
    
    # Third Tab
    
    tabItem(tabName = "M3",
            fluidPage(
              selectInput("Choice1",
                           label = "Select a Plot:",
                           choices = c("Seasonality", "Autocorrelation","Decomposition")),
              plotlyOutput("plots"),
              sliderInput("TrendSmooth",
                          label = "Smooth The Trend",
                          min = 1,
                          max = 500,
                          value = 6),
              numericInput("Year3",
                           label = "Input Starting Year",
                           value = 1969),
              numericInput("Year4",
                           label = "Input Ending Year",
                           value = 2004),
              fluidRow(
                tabBox(
                  title = "Seasonality",
                  side= "right",height = "250px",width=4,
                  selected = "Description",
                  tabPanel("Interpretation", "The seasonal pattern in the Canadian gas production can be seen by looking at the average amount of gas produced each month. The highest 
                           production months were December at 12.91 volumes and January at 12.62 volumes. Then the lowest production month was June at 10.36 gallons. 
                           In the beginning half of the year February, April, and May stay relatively consistent around 11.20 volumes with a small increase in March 
                           to 12.02 volumes. In the latter half of the year July, August, and September say consistent around 10.70 volumes. There are two small increases, 
                           one in October to 11.63 volumes and another one in November to 11.98 volumes. Additionally, from 1969 to 2004 there is a sharp increase in the 
                           number of volumes produced. "),
                  tabPanel("Description", "A seasonal plot shows all of the information collected on a subject over a certain period of time. 
                           Each point on the graph represents a number in units that corresponds to a season the data was collected. 
                           These points are then connected by a continuous line. By viewing this plot, you can view the seasonal pattern of the data. 
                           A seasonal pattern shows an increase or decrease depending on the season of the data. A season can be a quarter, month, week, etc.")
                ),
                tabBox(
                  title = "Autocorrelation",
                  side= "right",height = "250px",width=4,
                  selected = "Description",
                  tabPanel("Interpretation", "The autocorrelation plot for Canadian gas production has a positive relationship because the data follows itself. This means that if there is an 
                           increase then it continues to increase over time. Since gas production has a trend and the lag is one month, the acf is large and positive because 
                           the observations in past months are near the same value. These positive values slowly decrease as the lags increase because they are getting farther 
                           away from the last actual value. Additionally, the seasonal pattern of the data can be seen through the curved increases. This is because acf is higher 
                           for the seasonal lags compared to other lags. "),
                  tabPanel("Description", "An autocorrelation plot is used to determine how well a line fits the relationship between the lagged values in the data. 
                  Lagged values are the way that the data is grouped together. For example, a lag 1 would be the relationship between the first and second 
                  value while a lag 3 would be the relationship between the first and fourth variable. The autocorrelation coefficient, acf, is used to measure 
                  this relationship. The values range between -1 and 1. A value closer to -1 or 1 means the relationship is significant. Additionally, the blue 
                  dashed lines can be used to determine if the relationship is signficantly different from 0. If the vertical black lines are above the blue dashed 
                  lines then it is significant."),
                  
                ),
                tabBox(
                  title = "Autocorrelation",
                  side= "right",height = "250px",width=4,
                  selected = "Description",
                  tabPanel("Interpretation", "The decomposition plot for the Canadian gas production shows that the trend and season explain most of the full-time series plot. They are the two 
                  largest components in the plot. While the remainder, the part that cannot be explained, is only a small portion of the data. "),
                  tabPanel("Description", "A decomposition plot displays all the plots seperated by type. First is the full time series plot, then the trend plot, next is the seasonal plot, 
                  and finally the remainder. The gray boxes represent the amount of the full time plot that is explained by each part. The smaller the box, 
                  the more that part explains the data.")
                  
                )
              )
            )
    ),
    
    
    
    # Fourth tab
    
    tabItem(tabName = "M4",
            fluidPage(
            prettyCheckboxGroup(
              inputId = "Id032",
              label = "Choose:", 
              choices = c(MODELNAMES)
            ),
            plotOutput("plot3"),
            numericInput("num2", label = h5("Choose Time Period to Forecast"), value = 12),
            numericInput("Year6",
                         label = "Input Ending Year",
                         value = 2004),
            fluidRow(
              box("Forecasting analyzes past data collected on a single subject over a certain period of time. It then uses this information to make predictions about the future.
                  This information is useful when predicting the future trend and seasonal pattern of the data. Additonally, these predictions can then be used to determine what 
                  a company will do during certain seasons. For example, if a company sees that a certain month is lower then others, they can plan to reduce costs during this period 
                  and vice versa. When predicting values there is a prediction interval that represents the uncertainty in the prediction. The further out you predict, the wider it gets. 
                  While you can predict as far into the future as you want, it is wise to only predict a couple of years into the future.", width= 6,title= "Forecasting", status = "primary",solidHeader = T),
              tabBox(
                title = "Interpretations",
                side= "right",height = "250px",width=6,
                selected = "Naive",
                tabPanel("Drift","The drift model draws a line from the first value in the series to the last value in the series. The first value of the Canadian gas production was in June of 1969 at 4.23 volumes and the last value was in December of 2004 at 19.45. The forecast will predict this linear increase into the future."),
                tabPanel("Mean","The mean model predicts that the future value is equal to the average value of the historical data. The average value of the Canadian gas production was around 11.25 volumes. This same value will continue into the future for as many time periods as it is forecasted. "),
                tabPanel("Seasonal Naive", "The seaosnal naive model predicts the next value from the last value in the same season. The Canadian gas production stops in 2004. It predicts all the years after to have the same values in every month as 2004. January would be predicted to produce 19.24 volumes then 
                         February would be predicted to drop to 17.78 volumes. March would be expected to have a small increase in to 18.33 volumes with a small decrease to 17.85 in April. Then another 
                         small increase to 18.09 volumes in May with a sharp decrease in production to 17.17 in June. A sharp increase to 17.88 would be expected in July with a small decrease in August 
                         to 17.64 volumes. Then another sharp decrease in September to 16.91 volumes. From there it would increase until November where it levels off at 17.83 volumes and then another sharp 
                         increase in December to 19.45 volumes. "),
                tabPanel("Naive","The naive model predicts that the future values of the data will be equal to the last observed point in the dataset. In the Canadian gas production dataset, the last value was in December of 2004 at 19.45 volumes. This same prediction will continue into the future for as 
                         many time periods as it is forecasted.")
                
                
                
                
              )
              )
            )
    ),
    # Fifth tab 
    
    tabItem(tabName = "M5",
            fluidPage(
            prettyCheckboxGroup(
              inputId = "Id033",
              label = "Choose:", 
              choices = c(MODELNAMES2),
            ),
            plotOutput("plot4"),
            numericInput("num3", label = h5("Choose Time Period to Forecast"), value = 12),
            numericInput("Year7",
                         label = "Input Ending Year",
                         value = 2004),
            fluidRow(
              box("Forecasting analyzes past data collected on a single subject over a certain period of time. It then uses this information to make predictions about the future.
                  This information is useful when predicting the future trend and seasonal pattern of the data. Additonally, these predictions can then be used to determine what 
                  a company will do during certain seasons. For example, if a company sees that a certain month is lower then others, they can plan to reduce costs during this period 
                  and vice versa. When predicting values there is a prediction interval that represents the uncertainty in the prediction. The further out you predict, the wider it gets. 
                  While you can predict as far into the future as you want, it is wise to only predict a couple of years into the future.", width= 6,title= "Forecasting", status = "primary",solidHeader = T),
              tabBox(
                title = "Interpretations",
                side= "right",height = "250px",width=6,
                selected = "Holts",
                tabPanel("Holt Winters", "The Holts Winters model uses exponential smoothing. Exponential smoothing uses a weighted average that decreases expoentailly. If the alpha is higher, the recent past has more of an effect of the data. 
                         The Holts Winters model uses the recent linear trends and seasonal in the data to make its predicitons."),
                tabPanel("Holts","The Holts model uses exponential smoothing. Exponential smoothing uses a weighted average that decreases expoentailly. If the alpha is higher, the recent past has more of an effect of the data. 
                         The Holts model uses the recent linear trends in the data to make its predicitons.")
              )
            )
            
    )
    ),
    
    # Sixth tab 
    
    tabItem(tabName = "M6",
            fluidPage(
            prettyCheckboxGroup(
              inputId = "Id034",
              label = "Choose:", 
              choices = c(MODELNAMES3)
            ),
            plotOutput("plot5"),
            numericInput("num4", label = h5("Choose Time Period to Forecast"), value = 12),
            numericInput("Year8",
                         label = "Input Ending Year",
                         value = 2004),
            fluidRow(
              box("Forecasting analyzes past data collected on a single subject over a certain period of time. It then uses this information to make predictions about the future.
                  This information is useful when predicting the future trend and seasonal pattern of the data. Additonally, these predictions can then be used to determine what 
                  a company will do during certain seasons. For example, if a company sees that a certain month is lower then others, they can plan to reduce costs during this period 
                  and vice versa. When predicting values there is a prediction interval that represents the uncertainty in the prediction. The further out you predict, the wider it gets. 
                  While you can predict as far into the future as you want, it is wise to only predict a couple of years into the future.", width= 6,title= "Forecasting", status = "primary",solidHeader = T),
              tabBox(
                title = "Interpretations",
                side= "right",height = "250px",width=6,
                selected = "Manual Arima",
                tabPanel("Auto Arima","The arima model stands for autoregressive intergrated moving average. It has a seasonal and non-seasonal component. The autoregressive component deals with the lags, the intergrated compnent deals with the differencing,
                         and the moving average component deals with the errors. The auto arima values are selected automatically. In the nonseasonal component, the autoregressive is set to two, the integrated is set to zero, and the moving average is set to one. The seasonal component has an autoregressive of zero,
                         integrated of one, and moving average of one."),
                tabPanel("Manual Arima","The arima model stands for autoregressive intergrated moving average. It has a seasonal and non-seasonal component. The autoregressive component deals with the lags, the intergrated compnent deals with the differencing,
                         and the moving average component deals with the errors. The manual arima values are user selected. In the nonseasonal component, the autoregressive is set to one, the integrated is set to one, and the moving average is set to one. The seasonal component has an autoregressive of one,
                         integrated of one, and moving average of one.")
              )
            )
            
            )
    )
    
  ))
)


server <- function(input, output) {
  # Second tab: Full time series plot
  output$plot1 <- renderPlotly({
    Canadian_Gas %>%
      filter(year(Month) >= input$Year1) %>%
      filter(year(Month) <= input$Year2) %>%
      autoplot(Volume)+
      labs(y="Volume",
           x="Month",
           title="Canadian Gas Production")+
      easy_center_title() 
  })
  
  # Third tab: Seasonality, Autocorrelation,Decomposition
  output$plots <- renderPlotly({
    if(input$Choice1=="Seasonality"){ Canadian_Gas %>%
        filter(year(Month) >= input$Year3) %>%
        filter(year(Month) <= input$Year4) %>%
        gg_subseries(Volume)+
        theme(axis.text.x = element_text(angle=90))+
        labs(y="Volume",
             x="Month",
             title="Canadian Gas Production")+
        easy_center_title() 
    } else if (input$Choice1=="Autocorrelation"){
      Canadian_Gas %>%
        ACF(Volume) %>% autoplot()+ 
        labs(title = "Canadaian Gas Production")+
        easy_center_title()
    } else if(input$Choice1=="Decomposition"){
      ggplotly(
        Canadian_Gas %>%
          filter(year(Month) >= input$Year3) %>% 
          filter(year(Month) <= input$Year4) %>% 
          model(STL(Volume ~ trend(window = input$TrendSmooth) + 
                      season(window=6), robust=TRUE)) %>% 
          components() %>% 
          autoplot()+ 
          labs(title = "STL decomposition: Canadian Gas Production")+
          easy_center_title()
      )
    }
  })
  
  # Fourth tab: simple forecasting models
  output$plot3 <- renderPlot({ 
    (Canadian_Gas %>%
       autoplot(size=1)+
       labs(title = "Forecasting Canadian Gas Production",
            x="Month")+
       easy_center_title()+
       Canadian_Gas %>%
       filter(year(Month) <= input$Year6) %>%
       model(
         Naive = NAIVE(Volume),
         SeasonalNaive = SNAIVE(Volume),
         Mean = MEAN(Volume),
         Drift= RW(Volume ~ drift())
       ) %>%
       forecast(h=input$num2) %>%
       filter(.model %in% input$Id032) %>%
       autolayer(.model,size=1,level = NULL))+
      guides(colour=guide_legend(title="Forecast"))
    
  })
  
# Fifth tab: exponential smoothing 
  output$plot4 <- renderPlot({ 
    (Canadian_Gas %>%
       autoplot(size=1)+
       labs(title = "Forecasting Canadian Gas Production",
            x="Month")+
       easy_center_title()+
       Canadian_Gas %>%
       filter(year(Month) <= input$Year7) %>%
       model(
         Holt = ETS(Volume ~ error("A") + trend("A") + season("N")),
         HoltWinters= ETS(Volume ~ error("A") + trend("A") + season("A"))
       ) %>%
       forecast(h=input$num3) %>%
       filter(.model %in% input$Id033) %>%
       autolayer(.model,size=1,level = NULL))+
      guides(colour=guide_legend(title="Forecast"))
  })

# Sixth tab: arima
  output$plot5 <- renderPlot({ 
    (Canadian_Gas %>%
       autoplot(size=1)+
       labs(title = "Forecasting Canadian Gas Production",
            x="Month")+
       easy_center_title()+
       Canadian_Gas %>%
       filter(year(Month) <= input$Year8) %>%
       model(
         ManualArima= ARIMA(Volume~pdq(1,1,1) + PDQ(1,1,1)),
         AutoArima= ARIMA(Volume)
       ) %>%
       forecast(h=input$num4) %>%
       filter(.model %in% input$Id034) %>%
       autolayer(.model,size=1,level = NULL))+
      guides(colour=guide_legend(title="Forecast"))
})
}

shinyApp(ui = ui, server = server)