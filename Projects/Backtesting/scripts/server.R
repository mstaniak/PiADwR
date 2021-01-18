server <- function(input, output){
  observeEvent(input$show_help, {
    showModal(modalDialog("Informacje o parametrach"))
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
                        full_data = get_data(),
                        number_of_days = input$number_of_days,
                        min_momentum = input$min_momentum,
                        max_stocks = input$max_stocks,
                        min_inv_vola = input$min_inv_vola,
                        cashe = input$cash)
  })
  
  # dt <- run_simulation()
  
  output$equity_curve <- plotly::renderPlotly({
    dt <- run_simulation()
    equity_plot(dt$equity_history, input$plot_type)
  })
  
  output$summary_dt <- DT::renderDT({
    dt <- run_simulation()
    strat_summary(dt$equity_history)
  })
}
