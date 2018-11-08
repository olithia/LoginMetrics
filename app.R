
# title: "Final Project: Login Metrics Visualization"
# author: "Olithia Strom-Rhea"
# date: "Nov 6, 2018"

# This simple app plots a custom time series. The data represents user login counts by country by month to an unnamed webapp.
# Step 1: Enter a title for the chart
# Step 2: Toggle the Show/Hide Chart Title checkbox


library(shiny)
library(plotly)
library(tidyr)
library(dplyr)
library(tsbox)
library(plyr)

suppressPackageStartupMessages({library(plotly)})

ui <- fluidPage(fluidPage(
  titlePanel("Login Metrics Visualization"),
  em("This simple app plots a custom time series. The data represents user login counts by country by month over the past 6 months to an unnamed webapp."),
  h4("Step 1: Enter a title for the chart"),
  h4("Step 2: Toggle the Show/Hide Chart Title checkbox"),
  h4(""),
  sidebarLayout(
    sidebarPanel(
      textInput("title_text","Enter a title for the chart","Login Per Country"),
      checkboxInput("show_title", "Show/Hide Chart Title")
    ),
    mainPanel(
      plotlyOutput("plot1")
      
    )
  )
  
))

server <- function(input, output) {

  output$plot1 <- renderPlotly({
    #read in chart title from user
    main <- ifelse(input$show_title, input$title_text, "")
    
    #load data
    asean_logins_df <- readRDS("./asean_logins_df_new.RMD")
    #create time series
    Indonesia<-ts(as.numeric(as.vector(asean_logins_df[1,])), start=c(2018, 5), end=c(2018, 10), frequency=12)
    Malaysia<-ts(as.numeric(as.vector(asean_logins_df[2,])), start=c(2018, 5), end=c(2018, 10), frequency=12)
    Philippines<-ts(as.numeric(as.vector(asean_logins_df[3,])), start=c(2018, 5), end=c(2018, 10), frequency=12)
    Thailand<-ts(as.numeric(as.vector(asean_logins_df[4,])), start=c(2018, 5), end=c(2018, 10), frequency=12)
    VietNam<-ts(as.numeric(as.vector(asean_logins_df[5,])), start=c(2018, 5), end=c(2018, 10), frequency=12)
    x.ts<-ts_c(Indonesia,Malaysia,Philippines,Thailand,VietNam)
    #data("x.ts")
    metrics <- as.data.frame(x.ts) %>%
      gather(country, logins) %>%
      mutate(time = rep(time(x.ts), 5))
    #plot the data in a chart
    plot_ly(metrics, x = ~time, y = ~logins, color = ~country, type="scatter", mode="line") %>%
      layout(title = main)
    })

}

shinyApp(ui, server)