library(shiny)
library(shinydashboard)
library(shinyalert)
library(readr)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(quantmod)
library(MASS)
library(vroom)

Sys.setlocale("LC_TIME", "English")

source("functions.R")

getSymbols("BTC-USD", src = "yahoo")
dates <- index(`BTC-USD`)
d.day <- as.data.frame(`BTC-USD`)
d.day <- as.data.table(cbind(dates, d.day))
names(d.day) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
d.day <- na.omit(d.day)

shinyServer(function(input, output, session) {
    # bitcoin ----
    output$table_hist <- DT::renderDataTable({
        DT::datatable(d.day, options = list(paging = FALSE, searching = FALSE))
    })
    
    dateRange <- reactive({
        ZoomDataRange(d.day, input$radioButtons_Zoom, input$dateRangeInput_Date)
    })
    
    data_chart <- reactive({
        PrepareZoomDataOHLC(d.day, dateRange())
    })
    
    output$table <- DT::renderDataTable({
        GetZoomTableOHLC(data_chart())
    })
    
    output$line <- renderPlot({
        PlotLineChart(data_chart(), input$selectInput_OHLC)
    })
    
    observe({
        updateSelectInput(session = session, 
                          "numOfPeriods",
                          choices = 1:as.numeric(ZoomDataRange(d.day, 
                                                               input$radioButtons_Zoom, 
                                                               input$dateRangeInput_Date)[2] - ZoomDataRange(d.day, 
                                                                                                             input$radioButtons_Zoom, 
                                                                                                             input$dateRangeInput_Date)[1]))
    })
    
    output$candlesticks <- renderPlot({
        PlotCandlestickChart(data_chart(), input$selectInput_candlesticks, input$numOfPeriods)
    })
    
    data_rate <- reactive({
        GetRateData(d.day, input$selectInput_rate_type, input$selectInput_rate_period, dateRange())
    })
    
    output$line_rate <- renderPlot({
        PlotRate(data_rate())
    })

    output$high <- renderInfoBox({
        changeColor(d.day, "High")
    })
    
    output$low <- renderInfoBox({
        changeColor(d.day, "Low")
    })
    
    output$open <- renderInfoBox({
        changeColor(d.day, "Open")
    })
    
    output$close <- renderInfoBox({
        changeColor(d.day, "Close")
    })
    
    observeEvent(input$info_line, {
        showModal(modalDialog("The line chart is another way to visualize the price of the underlying, but unlike candlesticks, which allow you to analyze finer points of pricing, a line chart provides a quick way to visualize a longer-term trend. "))
    })
    
    observeEvent(input$info_rate, {
        showModal(modalDialog("A rate of return (RoR) is the net gain or loss of an investment over a specified time period, expressed as a percentage of the investment's initial cost. "))
    })
    
    observeEvent(input$info_candlesticks, {
        showModal(modalDialog(HTML("Candlestick patterns are used to predict the future direction of price movement. Green candlestick chart informs us that the price is increasing and red candlestick shows that the price is decreasing. <br> <br> A simple moving average (SMA) calculates the average of a selected range of prices, usually closing prices, by the number of periods in that range. A simple moving average is a technical indicator that can aid in determining if an asset price will continue or if it will reverse a bull or bear trend. <br> <br> The exponential moving average (EMA) is a technical chart indicator that tracks the price of an investment (like a stock or commodity) over time. <br> <br> The EMA is a type of weighted moving average (WMA) that gives exponentially more weighting or importance to recent price data. The weighted moving average (WMA) is a technical indicator that assigns a greater weighting to the most recent data points, and less weighting to data points in the distant past.  In case of this indicator the weights increase linearly.")))
    })
    
    # user data ----
    UserData <- eventReactive(input$upload,{
        inFile <- input$upload
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               validate("Invalid file. Please upload a .csv file.")
        )
        if (is.null(inFile)){
            returnValue()
        }
        else{
            d <- read.csv(inFile$datapath)
            d <- as.data.table(d)
            d[, Date := as.Date(Date)]
            d
        }
    })
    
    output$dates <- renderUI({
        dates <- as.Date(UserData()$Date)
        minval <- min(dates)
        maxval <- max(dates)

        dateRangeInput("dateRangeInput_Date_user_data", label = "Choose date range:",
                       start = minval, end = maxval,
                       min = minval, max = maxval)
    })
    
    dateRange_user_data <- reactive({
        ZoomDataRange(UserData(), input$radioButtons_Zoom_user_data, input$dateRangeInput_Date_user_data)
    })
    
    data_chart_user_data <- reactive({
        PrepareZoomDataOHLC(UserData(), dateRange_user_data())
    })
    
    output$table_user_data <- DT::renderDataTable({
        GetZoomTableOHLC(data_chart_user_data())
    })
    
    output$line_user_data <- renderPlot({
        PlotLineChart(data_chart_user_data(), input$selectInput_OHLC_user_data)
    })
    
    observe({
        updateSelectInput(session = session, 
                          "numOfPeriods_user_data",
                          choices = 1:as.numeric(ZoomDataRange(UserData(), 
                                                               input$radioButtons_Zoom_user_data, 
                                                               input$dateRangeInput_Date_user_data)[2] - ZoomDataRange(UserData(), 
                                                                                                             input$radioButtons_Zoom_user_data, 
                                                                                                             input$dateRangeInput_Date_user_data)[1]))
    })
    
    output$candlesticks_user_data <- renderPlot({
        PlotCandlestickChart(data_chart_user_data(), input$selectInput_candlesticks_user_data, input$numOfPeriods_user_data)
    })
    
    data_rate_user_data <- reactive({
        GetRateData(UserData(), input$selectInput_rate_type_user_data, input$selectInput_rate_period_user_data, dateRange_user_data())
    })
    
    output$line_rate_user_data <- renderPlot({
        PlotRate(data_rate_user_data())
    })
    
    observeEvent(input$info_line_user_data, {
        showModal(modalDialog("The line chart is another way to visualize the price of the underlying, but unlike candlesticks, which allow you to analyze finer points of pricing, a line chart provides a quick way to visualize a longer-term trend. "))
    })

    observeEvent(input$info_rate_user_data, {
        showModal(modalDialog("A rate of return (RoR) is the net gain or loss of an investment over a specified time period, expressed as a percentage of the investment's initial cost. "))
    })

    observeEvent(input$info_candlesticks_user_data, {
        showModal(modalDialog(HTML("Candlestick patterns are used to predict the future direction of price movement. Green candlestick chart informs us that the price is increasing and red candlestick shows that the price is decreasing. <br> <br> A simple moving average (SMA) calculates the average of a selected range of prices, usually closing prices, by the number of periods in that range. A simple moving average is a technical indicator that can aid in determining if an asset price will continue or if it will reverse a bull or bear trend. <br> <br> The exponential moving average (EMA) is a technical chart indicator that tracks the price of an investment (like a stock or commodity) over time. <br> <br> The EMA is a type of weighted moving average (WMA) that gives exponentially more weighting or importance to recent price data. The weighted moving average (WMA) is a technical indicator that assigns a greater weighting to the most recent data points, and less weighting to data points in the distant past.  In case of this indicator the weights increase linearly.")))
    })
})
