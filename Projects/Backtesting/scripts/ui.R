ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel('Backtesting'),
  # options(shiny.sanitize.errors = FALSE),
  add_busy_gif(
    src = "https://media.giphy.com/media/13HBDT4QSTpveU/giphy.gif",
    height = 70, width = 70, timeout = 1, position = 'full-page'
  ),
  use_waiter(),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Dane",
                 br(),
                 p('Wybierz zakres dat i spółki, które Cię interesują, a następnie wciśnij przycisk "Get data".'),
                 # br(),
                 dateInput(
                   inputId = 'start_date',
                   label = ('Data początkowa:'),
                   max = '2020-12-01',
                   value = '2020-01-01',
                   language = 'pl',
                   weekstart = 1
                 ),
                 
                 dateInput(
                   inputId = 'end_date',
                   label = ('Data końcowa:'),
                   max = '2020-12-01',
                   value = '2020-12-01',
                   language = 'pl',
                   weekstart = 1
                 ),
                 
                 pickerInput(
                   inputId = "tickers",
                   label = "Wybierz spółki:", 
                   choices = tickers,
                   options = list(
                     `actions-box` = TRUE,
                     `live-search` = TRUE,
                     `selected-text-format`= "count > 6",
                     `count-selected-text` = 'Wybrano {0} spółek',
                     `none-selected-text` = 'Nie wybrano żadnej spółki'
                   ),
                   multiple = TRUE
                 ),
                 
                 br(),
                 actionButton(
                   inputId = "get_data_button",
                   label = "Get data",
                   class = "btn-primary"
                   ),
                 ),
        
        tabPanel("Parametry",
                 br(),
                 sliderInput(
                   inputId = "rebalance_frequency",
                   label = "Częstość rebalansacji portfela:",
                   min = 1,
                   max = 3,
                   value = 1,
                   step = 1
                 ),
                 radioButtons(
                   inputId = "period",
                   label = "Częstość rebalansacji portfela cd.:",
                   choices = c("month", "week")
                 ),
                 numericInput(
                   inputId = "number_of_days",
                   label = "Ramy czasowe dla wyliczenia momentum:",
                   value = 125
                 ),
                 numericInput(
                   inputId = "min_momentum",
                   label = "Minimalne momentum:",
                   value = 40
                 ),
                 numericInput(
                   inputId = "max_stocks",
                   label = "Maksymalna liczba spółek w portfelu",
                   value = 10
                 ),
                 numericInput(
                   inputId = "min_inv_vola",
                   label = "Minimalna zmienność odwrócona:",
                   value = 30
                 ),
                 numericInput(
                   inputId = "cash",
                   label = "Początkowa gotówka:",
                   value = 10000
                 ),
                 numericInput(
                   inputId = "comission_rate",
                   label = "Prowizja brokerska:",
                   value = 0.0038
                 ),
                 br(),
                 actionButton(
                   inputId = "run_simulation_button",
                   label = "Symulacja!",
                   class = "btn-primary"
                 ),
                 actionButton(
                   inputId = "show_help",
                   label = "Informacje o parametrach",
                   class = "btn-info"
                 ),
        )
      )
    ),
    
    mainPanel(
      use_waiter(),
      tabsetPanel(
        tabPanel('Krzywa kapitału',
                 br(),
                 prettyRadioButtons(
                   inputId = "plot_type",
                   label = "Wybierz rodzaj wykresu:", 
                   choices = c("point", "col"),
                   inline = TRUE,
                   fill = TRUE
                 ),
                 plotly::plotlyOutput('equity_curve') %>% withSpinner(color = 'blue')),
        
        tabPanel('Dane szczegółowe',
                 DT::DTOutput('summary_dt') %>% withSpinner(color = 'blue')),
        
        tabPanel('Historia portfela',
                 DT::DTOutput('portfolio_dt') %>% withSpinner(color = 'blue'),
                 )
      )
    )
  )
)