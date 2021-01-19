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
  
  # rv <- reactiveValues(page = 1)
  # 
  # observe({
  #   toggleState(id = "prevBtn", condition = rv$page > 1)
  #   toggleState(id = "nextBtn", condition = rv$page < 10)
  #   hide(selector = ".page")
  #   show(paste0("step", rv$page))
  # })
  # 
  # navPage <- function(direction) {
  #   rv$page <- rv$page + direction
  # }
  # 
  # observeEvent(input$prevBtn, navPage(-1))
  # observeEvent(input$nextBtn, navPage(1))
  
  data_from_db <- reactiveValues(data = NULL)

  observeEvent(input$get_data_button, {
    data_from_db$data <- get_all_data(input$tickers, input$start_date, input$end_date)
  })

  rebalance_dates <- reactive({
    get_rebalance_dates(input$start_date, input$end_date, input$rebalance_frequency, input$period)
  })
  
  simulation <- reactiveValues()
  
  observeEvent(input$run_simulation_button, {
    simulation$results <- backtest_simulation(dt_from_db = data_from_db$data,
                                              commission_rate = input$comission_rate,
                                              rebalance_dates = rebalance_dates(),
                                              number_of_days = input$number_of_days,
                                              min_momentum = input$min_momentum,
                                              max_stocks = input$max_stocks,
                                              min_inv_vola = input$min_inv_vola,
                                              cash = input$cash)
  })
  
  output$equity_curve <- plotly::renderPlotly({
    req(simulation$results)
    equity_plot(simulation$results$equity_history, input$plot_type)
  })
  
  output$summary_dt <- DT::renderDT({
    req(simulation$results)
    DT::datatable(strat_summary(simulation$results$equity_history),
                  options = list(paging = FALSE, searching = FALSE))
  })
  
  output$portfolio_dt <- DT::renderDT({
    req(simulation$results)
    DT::datatable(rbindlist(simulation$results$portfolio_history),
                  options = list(paging = FALSE))
  })
}
