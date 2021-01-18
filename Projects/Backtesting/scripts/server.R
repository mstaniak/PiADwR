server <- function(input, output){
  observeEvent(input$show_help, {
    showModal(modalDialog(title = "Informacje o parametrach:",
                          HTML('Częstość rebalansacji portfela - zaznacz wartości, jak często chcesz 
                          poddawać rebalansacji swój portfel; 1 month oznacza rebalansację raz w miesiącu,
                          3 week oznacza rebalansację co 3 tygodnie itd.<br>
                          Ramy czasowe dla wyliczenia momentum - 
                          okres dla którego wyliczane jest momentum <br>
                          Minimalne momentum - minimalna wartość wskaźnika momentum <br>
                          Minimalna zmienność odwrócona - im większa tolerancja ryzyka 
                          tym mniejszą wartość należy podać <br>
                          Początkowa gotówka - podaj wartość gotówki jaką chcesz dysponować 
                          na początku symulacji <br>
                          Prowizja brokerska - podaj wartość prowizji brokerskiej, którą chcesz
                          uwzględnić w symulacji
                          ')))
  })
  
  data_from_db <- reactiveValues(data = NULL)
  
  observeEvent(input$get_data_button, {
    data_from_db$data <- get_all_data(input$tickers, input$start_date, input$end_date)
  })
  
  # get_data <- eventReactive(input$get_data_button, {
  #   get_all_data(input$tickers, input$start_date, input$end_date)
  # })
  
  rebalance_dates <- reactive({
    get_rebalance_dates(input$start_date, input$end_date, input$rebalance_frequency, input$period)
  })
  
  run_simulation <- eventReactive(input$run_simulation_button, {
    backtest_simulation(input$comission_rate,
                        rebalance_dates(),
                        data = get_data(),
                        number_of_days = input$number_of_days,
                        min_momentum = input$min_momentum,
                        max_stocks = input$max_stocks,
                        min_inv_vola = input$min_inv_vola,
                        cash = input$cash)
  })
  
  dt <- reactive(run_simulation())
  
  output$equity_curve <- plotly::renderPlotly({
    # dt <- run_simulation()
    equity_plot(dt$equity_history, input$plot_type)
  })
  
  output$summary_dt <- DT::renderDT({
    # dt <- run_simulation()
    strat_summary(dt$equity_history)
  })
}
