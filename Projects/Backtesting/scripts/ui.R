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
        choices = c('abe', 'acg', 'ago', 'alg', 'awm', 'aml', 'amb', 'apt', 'arh', 'atc', 'asb', 'abs', 
                    'ast', '1at', 'atg', 'apr', 'bio', 'lwb', 'bbt', 'brs', 'bos', 'cmp', 'cpg', 'dbc', 
                    'eex', 'ent', 'fro', 'fte', 'gop', 'gtn', 'gnb', 'glc', 'grn', 'hrp', 'ida', 'inc', 
                    'irl', 'kgn', 'ksw', 'ltx', 'lbw', 'mci', 'mdg', 'mnc', 'mrb', 'mlg', 'mls', 'net', 
                    'nwg', 'oat', 'opn', 'bkm', 'pcr', 'pxb', 'pep', 'psw', 'phn', 'pce', 'pxm', 'r22', 
                    'rbw', 'rvu', 'snk', 'slv', 'ska', 'stx', 'stp', 'tim', 'tor', 'toa', 'trk', 'ulg', 
                    'unt', 'vgo', 'vox', 'vrg', 'wwl', 'wlt', 'wse', 'zep', 'pbx'),
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format`= "count > 6",
          `count-selected-text` = 'Wybrano {0} spółek',
          `none-selected-text` = 'Nie wybrano żadnej spółki'
        ),
        multiple = TRUE
      ),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Krzywa kapitału'),
        tabPanel('Dane szczegółowe'),
        tabPanel('Historia portfela')
      )
    )
  )
)