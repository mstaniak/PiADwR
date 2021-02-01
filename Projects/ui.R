## ui.R ##
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

shinyUI(dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "Global terrorism"
    ),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu( ## tutaj tworzymy zakładki w menu, każdy ma osobne, ale może mieć kilka
      menuItem(
        text = "National statistics",
        tabName = "national_statistics"
        ),
      menuItem(
        text = "Attack type", 
        tabName = "gun_type"
        ),
      menuItem(
        text = "Yearly comparison",
        tabName = "Year_Analysis"
      ),
      menuItem(
        text = "Correlations",
        tabName = "correlations"
      ),
      menuItem(
        text = "Incidents on map",
        tabName = "maps"
      )
      )
    ),
  ## Body 
  dashboardBody(
    tabItems(
      # national_statistics ----
      tabItem(
        tabName = "national_statistics",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = 'filter', 
              label = 'filter by:', 
              selected = 'ncases', 
              choices = list(
                'number of cases (ncases)' = 'ncases',
                'number of killed (nkill)'='nkill',
                'number of terrorists killed (nkiller)' = 'nkillter',
                'number of wounded (nwound)' = 'nwound',
                'value of property damage (propvalue)' = 'propvalue')
              ),
            selectInput(
              inputId = 'sort', 
              label = 'sort:', 
              selected = 'TRUE', 
              choices = list('ascending' = 'FALSE','descending'= 'TRUE')
              ),
            sliderTextInput(
              inputId = "show",
              label = "show first n countries:", 
              choices = seq(from = 1,to = 50, by = 1),
              grid = TRUE
              ),
            pickerInput(
              inputId = "countries",
              label = "select country:", 
              choices = countries_list,
              options = list(`actions-box` = TRUE), 
              multiple = TRUE
              ),
            actionButton(
              inputId = "go_button_ns", 
              label = "generate!")
            ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = 'Plot',
                plotOutput(
                  outputId = 'plot_national_statisticks_table',
                  height = "650px"
                  )
                ),
              tabPanel(
                title = 'Table', 
                tableOutput(
                  outputId = 'table_national_statisticks_table'
                  )
                )
              )
            )
          )
        ),
      # correlations ----
      tabItem(
        tabName = "correlations",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = 'correlations_dependent', 
              label = 'dependent variable (Y):', 
              selected = "nkillter", 
              choices = correlated_variables
              ),
            selectInput(
              inputId = 'correlations_explanatory', 
              label = 'explanatory variable (X):', 
              selected = 'nkill', 
              choices = correlated_variables
              ),
            selectInput(
              inputId = 'correlations_predict_line', 
              label = 'Predict lines:', 
              selected = 'none', 
              choices = c("none","lm", "gam")
              ),
            pickerInput(
              inputId = "correlations_countries",
              label = "select country:", 
              choices = countries_list,
              options = list(`actions-box` = TRUE), 
              multiple = TRUE
            ),
            selectInput(
              inputId = 'correlations_group_by', 
              label = 'group_by:', 
              selected = 'none', 
              choices = list('none' = 'NULL',
                             'attack type' = 'attacktype1_txt',
                             'weapon type'= 'weaptype1_txt', 
                             'region' = 'region_txt',
                             'country' = 'country_txt')
            ),
            actionButton(
              inputId = "go_button_cor", 
              label = "generete!"
              )
            ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = 'Plot', 
                plotOutput(
                  outputId = 'plot_correlations',
                  height = "650px")
                )
              )
            )
          )
        ),
      # attack type ----
      tabItem(
        tabName = "gun_type",
        sidebarLayout(
          sidebarPanel(
            # COUNTRY mini tab
            h4('COUNTRY'),
            selectInput(
              inputId = 'sort_att',
              label = 'sort:',
              selected = 'TRUE',
              choices = list('ascending' = 'FALSE',
                             'descending'= 'TRUE')
            ),
            selectInput(
              inputId = "countries_att",
              label = "select country:",
              choices = countries_list
            ),
            actionButton(
              inputId = "go_button_att",
              label = "generete!"
            ),
            # REGION mini tab
            h4('REGION'),
            selectInput(
              inputId = 'sort_att_r',
              label = 'sort:',
              selected = 'TRUE',
              choices = list('ascending' = 'FALSE',
                             'descending'= 'TRUE')
            ),
            selectInput(
              inputId = "regions_att",
              label = "select region:",
              choices = regions_list
            ),
            actionButton(
              inputId = "go_button_att_r",
              label = "generete!"
            )
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                'Plot country', 
                plotOutput(
                  outputId = 'plot_guns_per_country'
                  )
                ),
              tabPanel(
                title = 'Table country', 
                tableOutput(
                  outputId = 'table_att_per_country'
                  )
                ),
              tabPanel(
                'Plot region', 
                plotOutput(
                  outputId = 'plot_guns_per_region'
                  )
                ),
              tabPanel(
                title = 'Table region', 
                tableOutput(
                  outputId = 'table_att_per_region'
                  )
                )
              )
            )
          )
        ),
      # maps ----
      tabItem(
        tabName = "maps",
        sidebarLayout(
          sidebarPanel(
            tags$head(
              tags$style(
              HTML(".shiny-notification {
              position:fixed;
              top: calc(10%);
              left: calc(50%);
              }"
                   )
              )
              ),
            pickerInput(
              inputId = "countries_map",
              label = "Select countries:", 
              choices = countries_list,
              options = list(`actions-box` = TRUE), 
              multiple = TRUE
              ),
            sliderInput(
              inputId = "years_map",
              label = "Choose years:", 
              min = 1970,
              max = 2018,
              value = c(2000,2010),
              sep = ""
              ),
            radioGroupButtons(
              inputId = "map_type",
              label = "Choose map type:", 
              choices = c("Markers", "Circles"),
              status = "primary"
              ),
            actionButton(
              inputId = "show_map", 
              label = "Show"
              )
            ),
          mainPanel(
            leafletOutput(
              outputId = "map", 
              height = 800
              )
            )       
          )
        ),
      # years ----
      tabItem(
        tabName = "Year_Analysis",
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              inputId = "years",
              label = "choose the year interval you're interested in:", 
              min = 1970,
              max = 2018,
              value = c(1990,2010),
              sep = ""
            ),
            checkboxGroupInput(
              inputId = "Regions", 
              label = "check selected regions:",
              choices = regionTotal$region, 
              selected = 'Oceania'
            )#,
            # actionButton(
            #   inputId = "selectAll", 
            #   label = "Show the analysis for all the years"
            #   )
            ),
          mainPanel(
            tabsetPanel(
                  tabPanel(
                    title = 'Timely analysis',
                    plotOutput(
                      outputId = 'PlotYear'
                      )
                    ),
                  tabPanel(
                    title = 'Total Attack Amount', 
                    plotOutput(
                      outputId = 'PlotRegions'
                      )
                    )
                  )
            )
          )
        )
      )
    )
  )
  )

