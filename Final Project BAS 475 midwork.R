library(shiny)
library(fpp3)
library(ggplot2)
library(shinyWidgets)
library(quantmod)
library(plotly)
library(dplyr)
library(shinydashboard)
library(tidyquant)
library(flexdashboard)
library(feasts)
library(ggeasy)
library(ggthemes)
library(seasonal)
library(shinythemes)
library(bslib)
library(tsibble)



########### CREATE TSIBBLE ###########


# Path where data is
file_path <- "CriminalMindsData.csv"
# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
g_trends$Interest <- as.numeric(ifelse(g_trends$Interest == "<1",
                                       0, g_trends$Interest))
# Convert to tsibble
g_trends <- tsibble(g_trends)
autoplot(g_trends) +
  geom_vline(xintercept = as.numeric(as.Date("2006-05-10")),
             linetype = 4, colour = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2007-05-16")),
             linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2008-05-21")),
             linetype = 4, colour = "yellow") +
  geom_vline(xintercept = as.numeric(as.Date("2009-05-20")),
             linetype = 4, colour = "purple") +
  geom_vline(xintercept = as.numeric(as.Date("2010-05-26")),
             linetype = 4, colour = "orange") +
  geom_vline(xintercept = as.numeric(as.Date("2011-05-18")),
             linetype = 4, colour = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2012-05-16")),
             linetype = 4, colour = "pink") +
  geom_vline(xintercept = as.numeric(as.Date("2013-05-22")),
             linetype = 4, colour = "violet") +
  geom_vline(xintercept = as.numeric(as.Date("2014-05-14")),
             linetype = 4, colour = "green") +
  geom_vline(xintercept = as.numeric(as.Date("2015-05-06")),
             linetype = 4, colour = "maroon") +
  geom_vline(xintercept = as.numeric(as.Date("2016-05-04")),
             linetype = 4, colour = "#FF8200") +
  geom_vline(xintercept = as.numeric(as.Date("2017-05-10")),
             linetype = 4, colour = "navy") +
  geom_vline(xintercept = as.numeric(as.Date("2018-04-18")),
             linetype = 4, colour = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2019-02-06")),
             linetype = 4, colour = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2020-02-19")),
             linetype = 4, colour = "yellow") +
  ggtitle("Start of a Criminal Mind's Season - Search Frequency") +
  easy_center_title() -> P
ggplotly(P)


########### UI ############


ui <-
  dashboardPage( skin = "purple",
                 dashboardHeader(title = "Interest in The Show \"Criminal Minds\"", titleWidth = 600),
                 dashboardSidebar( width = 200,
                                   sidebarMenu(
                                     menuItem("Introduction", tabName = "intro", 
                                              icon = icon("dashboard")),
                                     menuItem("Full-Time Series", tabName = "graph1", 
                                              icon = icon("chart-line")),
                                     menuItem("Pick Your Plot", tabName = "graph2", 
                                              icon = icon("chart-line")),
                                     menuItem("Subseries Plot", tabName = "feature", 
                                              icon = icon("chart-line")),
                                     menuItem("Simple Models", tabName = "graph3", 
                                              icon = icon("chart-line")),
                                     menuItem("Exponential Smoothing", tabName = "graph4", 
                                              icon = icon("chart-line")),
                                     menuItem("ARIMA", tabName = "graph5", 
                                              icon = icon("chart-line"))
                                   )
                 ),
                 dashboardBody(
                   tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 28px;
      }
    '))),
                   
                   tabItems(
                     ### First tab ###
                     
                     tabItem(tabName = "intro",
                             h1("Introduction - Alexis Weigel"), 
                             
                             hr(),
                             
                             tags$div(
                               tags$h3("This app analyzes the interest in the show 
                                      \"Criminal Minds\" from January 2004 to March 2022."),
                               
                               tags$head(tags$style('h3 {color:"purple";}')),
                               
                               tags$br(),
                               
                               tags$h3("* The 2nd tab displays the Full-Time Series plot 
                                       for the interest in The Show \"Criminal Minds\" from January 2004 to March 2022."),
                               
                               tags$br(),
                               
                               tags$h3("* The 3rd tab displays your choice in one of three types of 
                                       graphics: seasonality, autocorrelation, and decomposition. "),
                               
                               tags$br(),
                               
                               tags$h3("* The 4th tab displays a subseries plot and how each month varies for the interest 
                                       in The Show \"Criminal Minds\" from January 2004 to March 2022."),
                               
                               tags$br(),
                               
                               tags$h3("* The 5th tab displays your choice in one of four types of 
                                       simple models: Naive, Seasonal Naive, Mean and Drift. "),
                               
                               tags$br(),
                               
                               tags$h3("* The 6th tab displays your choice in one of two types of 
                                       exponential smoothing models: Holts and Hols/Winters. "),
                               
                               tags$br(),
                               
                               tags$h3("* The 7th tab displays your choice in one of two types of 
                                       ARIMA models: Manually Selected and Auto Selected. "),
                               
                               tags$br(),
                               
                             ),
                             
                     ),
                     
                     ### Second tab ###
                     
                     tabItem(tabName = "graph1",
                             h1("Full-Time Series Graph"),
                             
                             hr(),
                             
                             basicPage(
                               plotlyOutput("fulltimeseries")
                             ),
                             
                             hr(),
                             
                             h3("Interpretation"),
                             
                             h4("The full-time series shows a trend that was 
                                relatively increasing from 2005 - 2009. The trend
                                then appears to be about steady from about
                                2011 - 2015. The trend then appears to decrease and be steady  
                                from 2016 - 2019. Then it starts to decrease  
                                from 2020 - present. There appears to be strong
                                seasonality throughtout the plot. This is 
                                likely due to the popularity of the show when it came out 
                                and when new seasons tend to come out every year or so. The vertical colored dotted lines 
                                on the graph represent when each new season came out.")
                             
                     ), 
                     
                     ### Third tab ###
                     
                     tabItem(tabName = "graph2",
                             h1("Graphic of Your Choice"), 
                             
                             hr(),
                             
                             radioButtons("plot_type", 
                                          label = h2("Which plot do you want to see?"),
                                          choices = c("Seasonality", 
                                                      "Autocorrelation", 
                                                      "Decomposition")),
                             
                             hr(),
                             
                             plotOutput("myplot"),
                             
                             hr(),
                             
                             h3("Interpretation"),
                             
                             textOutput("myplotint")
                             
                     ),
                     
                     ### Fourth tab ###
                     
                     tabItem(tabName = "feature",
                             h1("Subseries Plot"),
                             
                             hr(),
                             
                             basicPage(
                               plotlyOutput("subseries")
                             ),
                             
                             hr(),
                             
                             h3("Interpretation"),
                             
                             h4("The subseries plot shows blue horizontal lines that indicate 
                                the average interests for each month. This form of plot enables the 
                                underlying seasonal pattern to be seen clearly, and also 
                                shows the changes in seasonality over time. It shows 
                                the changes between different months/season, changes within
                                a particular month/season over time. Janurary, March, and July are shown as the 
                                the most seasonal and the averages are all pretty similar based on month.")
                     ),
                     
                     ### Fifth tab ###
                     
                     tabItem(tabName = "graph3",
                             h1("Simple Model of Your Choice"), 
                             
                             hr(),
                             
                             radioButtons("plot_type2", 
                                          label = h2("Which Simple Model Do You Want to See?"),
                                          choices = c("Naive", 
                                                      "Seasonal Naive", 
                                                      "Mean",
                                                      "Drift")),
                             
                             hr(),
                             
                             plotOutput("simple_model"),
                             
                             hr(),
                             
                             h3("Interpretation"),
                             
                             textOutput("simple_model_int")
                             
                     ),
                     ### Sixth tab ###
                     
                     tabItem(tabName = "graph4",
                             h1("Exponential Smoothing Model of Your Choice"), 
                             
                             hr(),
                             
                             radioButtons("plot_type3", 
                                          label = h2("Which Model Do You Want to See?"),
                                          choices = c("Holts", 
                                                      "Holts/Winters")),
                             
                             hr(),
                             
                             plotOutput("smoothing"),
                             
                             hr(),
                             
                             h3("Interpretation"),
                             
                             textOutput("smoothing_int")
                             
                     ),
                     
                     ### Seventh tab ###
                     
                     tabItem(tabName = "graph5",
                             h1("ARIMA Model of Your Choice"), 
                             
                             hr(),
                             
                             radioButtons("plot_type4", 
                                          label = h2("Which Model Do You Want to See?"),
                                          choices = c("Manually Selected", 
                                                      "Auto Selected")),
                             
                             hr(),
                             
                             plotOutput("arima"),
                             
                             hr(),
                             
                             h3("Interpretation"),
                             
                             textOutput("arima_int")
                             
                     )
                     
                     
                     
                   )  
                   
                 )
                 
  )

########### SERVER ############

### Time Series ###

server <- function(input, output, session) {
  output$fulltimeseries <- renderPlotly({
    p <- ggplot(g_trends, aes(Month, Interest)) + 
      geom_line(color = "black") + 
      theme_fivethirtyeight()+
      labs(title = "The Interest of The Show \"Criminal Minds\"", y = "Interest") +
      ggeasy::easy_center_title() +
      ggeasy::easy_all_text_color(color = "black") 
    ggplotly(P)
    
  })
  
  
### One of 3 plots ###
  
  output$myplot <- renderPlot({
    if (input$plot_type == "Seasonality") {
      g_trends %>% gg_season(Interest)+
        theme_fivethirtyeight()+
        labs(title = "The Interest of The Show \"Criminal Minds\"", y = "Interest") +
        ggeasy::easy_center_title() +
        ggeasy::easy_all_text_color(color = "black") 
      
    } 
    
    else if (input$plot_type == "Autocorrelation") {
      g_trends %>% ACF() %>% 
        autoplot()+
        labs(title = "Interest of The Show \"Criminal Minds\"")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "black") 
    }
    
    else if (input$plot_type == "Decomposition") {
      x11_dcmp <- g_trends %>%
        model(x11 = X_13ARIMA_SEATS(Interest ~ x11())) %>%
        components()
      autoplot(x11_dcmp) +
        labs(title = "Decomposition of Interest of The Show\"Criminal Minds\" using X-11.")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "black")
    }
  })
  
  output$myplotint <- renderText({
    if (input$plot_type == "Seasonality") {
      noquote(paste(c("The seasonality plot shows that the interest 
      in the show \"Criminal Minds\" peaks from January until March and 
      again from June to August. In March 2015, there is a huge jump in interest, 
      but a huge dip in March 2019 and March 2020. This is probably the cause of 
      COVID-19 and how popular the show is.", 
                      collapse = " ")))
    } 
    else if (input$plot_type == "Autocorrelation") {
      noquote(paste(c("The autocorrelation plot shows that the 
      interest in the show \"Criminal Minds\" is slowing decreasing as the lags
      increase that is due to seasonality, while the scalloped shape is due to the seasonality. This
      is likely due to when new seasons come out and the popularity of the show.", collapse = " ")))
    }
    else if (input$plot_type == "Decomposition") {
      noquote(paste(c("The X11 decomposition plot shows that the fall in the data
      in about 2013. The plot also shows a consistent amount
      of seasonality. Each grey bar represents the same length but because 
      the plots are on different scales, the bars vary in size. The large grey 
      bar in the bottom panel shows that the variation in the remainder component 
      is smallest compared to the variation in the data.", collapse = " ")))
    }
  })
  
  
### Subseries plot ###
  
  output$subseries <- renderPlotly({
    b <- g_trends %>%
      gg_subseries(Interest) +
      labs(
        y = "Interest",
        title = "Criminal Minds Interest") +
      ggeasy::easy_center_title() +
      ggeasy::easy_all_text_color(color = "black")
    ggplotly(b)
    
})
  

  ### One of 4 simple models ###
  
  output$simple_model <- renderPlot({
    if (input$plot_type2 == "Naive") {
      g_trends %>% model(NAIVE(Interest))+
        autoplot()+
        theme_fivethirtyeight()+
        labs(title = "The Naive Model of The Show \"Criminal Minds\"", y = "Interest") +
        ggeasy::easy_center_title() +
        ggeasy::easy_all_text_color(color = "black") 
      
    } 
    
    else if (input$plot_type2 == "Seasonal Naive") {
      g_trends %>%
        model(SNAIVE(Interest ~ lag("year")))%>%
        forecast(h = 10 )%>%
        autoplot(g_trends)+
        labs((title="SNAIVE Forecast of The Show \"Criminal Minds\""), 
             subtitle = "10 Year Forecast", 
             xlab="Year" )+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "black") 
    }
    
    else if (input$plot_type2 == "Mean") {
      g_trends %>% model(MEAN(Interest)) %>%
      autoplot() +
        labs(title = "Mean Model of Interest of The Show\"Criminal Minds\"")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "black")
    }
    
    else if (input$plot_type2 == "Drift") {
      g_trends %>%
        model(RW(Interest ~ drift())) %>%
        forecast(h = "5 years") %>%
        autoplot(g_trends) +
        labs(y = "Number of Interest", title = "Drift Model of The Show \"Criminal Minds\"")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "black") 
    }
  })
  
  output$simple_model_int <- renderText({
    if (input$plot_type2 == "Naive") {
      noquote(paste(c("The seasonality plot shows that the interest 
      in the show \"Criminal Minds\" peaks from January until March and 
      again from June to August. In March 2015, there is a huge jump in interest, 
      but a huge dip in March 2019 and March 2020. This is probably the cause of 
      COVID-19 and how popular the show is.", 
                      collapse = " ")))
    } 
    else if (input$plot_type2 == "Seasonal Naive") {
      noquote(paste(c("The autocorrelation plot shows that the 
      interest in the show \"Criminal Minds\" is slowing decreasing as the lags
      increase that is due to seasonality, while the scalloped shape is due to the seasonality. This
      is likely due to when new seasons come out and the popularity of the show.", collapse = " ")))
    }
    else if (input$plot_type2 == "Mean") {
      noquote(paste(c("The X11 decomposition plot shows that the fall in the data
      in about 2013. The plot also shows a consistent amount
      of seasonality. Each grey bar represents the same length but because 
      the plots are on different scales, the bars vary in size. The large grey 
      bar in the bottom panel shows that the variation in the remainder component 
      is smallest compared to the variation in the data.", collapse = " ")))
    }
    else if (input$plot_type2 == "Drift") {
      noquote(paste(c("The autocorrelation plot shows that the 
      interest in the show \"Criminal Minds\" is slowing decreasing as the lags
      increase that is due to seasonality, while the scalloped shape is due to the seasonality. This
      is likely due to when new seasons come out and the popularity of the show.", collapse = " ")))
    }
  })    
}

#### Run App ####

shinyApp(ui, server)

