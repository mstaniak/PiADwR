library(shiny)
library(shinydashboard)
library(dashboardthemes)

load("C:/Users/rogow/OneDrive/Dokumenty/UWr/Programowanie i analiza danych w R/BTC/theme.RData")

convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    mi
}

header <- dashboardHeader(
    title = list(span(img(src="btcLOGO.png", width = 100), span(img(src="btcGIF.gif", width = 50))))
)

sidebar <- dashboardSidebar(
    sidebarMenu(id = "menu1",
                convertMenuItem(
                    menuItem("Chart", 
                             tabName = "item1",
                             icon = icon("chart-line"),
                             radioButtons("radioButtons_Zoom",
                                          "Zoom",
                                          choices = c("3D", "7D", "1M", "3M", "6M", "1Y", "All", "Range"),
                                          selected = "1M"),
                             dateRangeInput("dateRangeInput_Date", 
                                            "Date range:",
                                            start  = "2014-09-17",
                                            end    = Sys.Date(),
                                            min    = "2014-09-17",
                                            max    = Sys.Date(),
                                            format = "yyyy-mm-dd")
                             ), "item1"),
                convertMenuItem(
                    menuItem("Historical Data",
                             tabName = "item2",
                             icon = icon("history")
                             ), "item2"),
                convertMenuItem(
                    menuItem("Data",
                             tabName = "item3",
                             icon = icon("file"),
                             fileInput('upload', 'Choose file to upload',
                                       accept = c(
                                           'text/csv',
                                           'text/comma-separated-values',
                                           '.csv'
                                       )),
                             radioButtons("radioButtons_Zoom_user_data",
                                          "Zoom",
                                          choices = c("3D", "7D", "1M", "3M", "6M", "1Y", "All", "Range"),
                                          selected = "All"),
                             uiOutput("dates")
                    ), "item3")
                
    )
)

body <- dashboardBody(
    tags$head(tags$style(HTML('.info-box {min-height: 100px;} .info-box-icon {height: 100px; line-height: 100px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    customTheme,
    tabItems(
        tabItem(tabName = "item1",
            fluidRow(infoBoxOutput("high", width = 3),
                     infoBoxOutput("low", width = 3),
                     infoBoxOutput("open", width = 3),
                     infoBoxOutput("close", width = 3)),
            br(),
            br(),
            tabBox(width = 12,
                   tabPanel("Line",
                            fluidRow(
                                column(3, selectInput("selectInput_OHLC", 
                                                      "Choose Price", 
                                                      choices = list("Open", "High", "Low", "Close"), 
                                                      selected = "Open"))),
                            plotOutput("line"),
                            actionButton("info_line", "Info")),
                   tabPanel("Rates Of Return",
                            fluidRow(
                                column(3, selectInput("selectInput_rate_type", 
                                                      multiple = FALSE,
                                                      "Select type",
                                                      choices = list("simple return" = "arithmetic",
                                                                     "logarithmic return" = "log"),
                                                      selected = "arithmetic")),
                                column(3, selectInput("selectInput_rate_period", 
                                                      multiple = FALSE,
                                                      "Select period",
                                                      choices = list("daily",
                                                                     "weekly",
                                                                     "monthly",
                                                                     "quarterly",
                                                                     "yearly",
                                                                     "all"),
                                                      selected = "daily"))),
                            plotOutput("line_rate"),
                            actionButton("info_rate", "Info")),
                   tabPanel("Candlestick",
                            fluidRow(
                                column(3, 
                                       selectInput("selectInput_candlesticks", 
                                                   multiple = FALSE,
                                                   "Select chart",
                                                   choices = list("Candlestick chart with simple moving average" = 1,
                                                                  "Candlestick chart with weighted moving average" = 2,
                                                                  "Candlestick chart with exponentinal moving average" = 3),
                                                   selected = 1), align = "center"),
                                column(3, selectInput("numOfPeriods", 
                                                      "Choose number of periods:", 
                                                      choices = c(1, 5)))),
                            plotOutput("candlesticks"),
                            actionButton("info_candlesticks", "Info")),
                   tabPanel("Table",
                            DT::dataTableOutput("table"),style = "height:500px;overflow-x: scroll;")
            )
        ),
        tabItem(tabName = "item2",
                box(width = 12,
                    DT::dataTableOutput("table_hist"),style = "height:500px;overflow-x: scroll;")
                ),
        tabItem(tabName = "item3",
                fluidRow(infoBoxOutput("high_user_data", width = 3),
                         infoBoxOutput("low_user_data", width = 3),
                         infoBoxOutput("open_user_data", width = 3),
                         infoBoxOutput("close_user_data", width = 3)),
                br(),
                br(),
                tabBox(width = 12,
                       tabPanel("Line",
                                fluidRow(
                                    column(3, selectInput("selectInput_OHLC_user_data", 
                                                          "Choose Price", 
                                                          choices = list("Open", "High", "Low", "Close"), 
                                                          selected = "Open"))),
                                plotOutput("line_user_data"),
                                actionButton("info_line_user_data", "Info")),
                       tabPanel("Rates Of Return",
                                fluidRow(
                                    column(3, selectInput("selectInput_rate_type_user_data", 
                                                          multiple = FALSE,
                                                          "Select type",
                                                          choices = list("simple return" = "arithmetic",
                                                                         "logarithmic return" = "log"),
                                                          selected = "arithmetic")),
                                    column(3, selectInput("selectInput_rate_period_user_data", 
                                                          multiple = FALSE,
                                                          "Select period",
                                                          choices = list("daily",
                                                                         "weekly",
                                                                         "monthly",
                                                                         "quarterly",
                                                                         "yearly",
                                                                         "all"),
                                                          selected = "daily"))),
                                plotOutput("line_rate_user_data"),
                                actionButton("info_rate_user_data", "Info")),
                       tabPanel("Candlestick",
                                fluidRow(
                                    column(3, 
                                           selectInput("selectInput_candlesticks_user_data", 
                                                       multiple = FALSE,
                                                       "Select chart",
                                                       choices = list("Candlestick chart with simple moving average" = 1,
                                                                      "Candlestick chart with weighted moving average" = 2,
                                                                      "Candlestick chart with exponentinal moving average" = 3),
                                                       selected = 1), align = "center"),
                                    column(3, selectInput("numOfPeriods_user_data", 
                                                          "Choose number of periods:", 
                                                          choices = c(1, 5)))),
                                plotOutput("candlesticks_user_data"),
                                actionButton("info_candlesticks_user_data", "Info")),
                       tabPanel("Table",
                                DT::dataTableOutput("table_user_data"),style = "height:500px;overflow-x: scroll;")
                )
        )
        
    )
)

shinyUI(
    dashboardPage(
        header,
        sidebar,
        body
    )
)




