ui <- fluidPage(
  titlePanel('Backtesting'),
  
  sidebarLayout(
    sidebarPanel(
      dateInput(
        inputId = 'start_date',
        label = ('Data początkowa:'),
        max = '2020-12-01',
        value = '2020-12-01',
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
      actionButton(
        inputId = "get_data_button",
        label = "Pobierz dane z bazy"
      ),
      numericInput(
        inputId = "number_of_days",
        label = "Podaj ramy czasowe dla wyliczenia momentum",
        value = 125
      ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Krzywa kapitału', 
                 selectInput(inputId = "plot_type",
                             label = "Wybierz rodzaj wykresu",
                             choices = c("point", "col"))),
        tabPanel('Dane szczegółowe'),
        tabPanel('Historia portfela')
      )
    )
  )
)
