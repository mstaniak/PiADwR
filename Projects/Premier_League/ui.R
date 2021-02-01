

shinyUI(fluidPage(
    theme = shinytheme("flatly"),
    
    navbarPage(title = img(src = "PL.png", 
                           width = 120),
               
               #### Ocena bukmachera (pre-match)  ####             
               
               tabPanel("Pre-match Analysis",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("first_page_select_home_team", "Home Team:",
                                            choices = teams,
                                            selected = "Chelsea"),
                                selectInput("first_page_select_away_team", "Away Team:",
                                            choices = teams,
                                            selected = "Liverpool"),
                                sliderInput("first_page_last_matches", "Number of last matches to show:",
                                            min = 3,
                                            max = 10,
                                            value = 5),
                                actionButton("run_prediction", "Confirm")
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Prediction",
                                             fluidRow(
                                                 column(width = 3,
                                                        fluidRow(tableOutput("expected_goals")),
                                                        fluidRow(tableOutput("results_distribution"))),
                                                 column(width = 9,
                                                        plotOutput("plot_results_distribution"))
                                             ),
                                             fluidRow(dataTableOutput("face_to_face_results_table")
                                             )
                                    ),
                                    tabPanel("Last Games",
                                             fluidRow(
                                                 column(width = 6,
                                                        plotOutput("home_team_results_distribution_plot")),
                                                 column(width = 6,
                                                        plotOutput("away_team_results_distribution_plot"))),
                                             fluidRow(
                                                 column(width = 6,
                                                        dataTableOutput("home_team_last_results_table")),
                                                 column(width = 6,
                                                        dataTableOutput("away_team_last_results_table"))
                                             )
                                    )
                                )
                            )
                        )
               ),
               #### Analiza Sezonu ####               
               
               tabPanel("Season Summary",
                        fluidRow(
                            column(width = 6,
                                   style = "background-color: #DFE6EB",
                                   column(width = 5, 
                                          selectInput("second_page_selected_season_input", "Season:",
                                                      choices = seasons,
                                                      selected = max(seasons))
                                   ),
                                   column(width = 1, 
                                          actionButton("run_table_for_season_button", "Confirm",
                                                       style = ("margin-top: 20px"))
                                   )
                            ),
                            column(width = 6,
                                   style = "background-color: #DFE6EB",
                                   column(width = 5, 
                                          selectInput("second_page_selected_stat_input", "Statistic:",
                                                      choices = c("Points" = "Pts",
                                                                  "Wins" = "W",
                                                                  "Draws" = "D",
                                                                  "Loses" = "L",
                                                                  "Goals Scored" = "GS",
                                                                  "Goals Conceded" = "GL",
                                                                  "Shots (Avg)" = "shots",
                                                                  "Rival Shots (Avg)" = "rival shots",
                                                                  "Shots On Target (Avg)" = "shots on target",
                                                                  "Rival Shots On Target (Avg)" = "rival shots on target",
                                                                  "Shots Accuracy" = "shots accuracy",
                                                                  "Fouls Committed (Avg)" = "fouls",
                                                                  "Fouls Suffered (Avg)" = "fouled",
                                                                  "Corners (Avg)" = "corners",
                                                                  "Yellow Cards" = "YC",
                                                                  "Red Cards" = "RC"),
                                                      selected = "Pts")
                                   ),
                                   column(width = 1, 
                                          actionButton("run_plot_for_stat_button", "Confirm",
                                                       style = ("margin-top: 20px"))
                                   )
                            )
                        ),
                        fluidRow(
                            column(width = 6,
                                   dataTableOutput("season_table")
                            ),
                            column(width = 6,
                                   plotOutput("season_stat")
                            )
                        )
               ),
               
               #### porownanie druzyn ####              
               
               tabPanel("Teams Comparison",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("third_page_select_home_team", "Home Team:",
                                            choices = teams,
                                            selected = "Chelsea"),
                                selectInput("third_page_select_away_team", "Away Team:",
                                            choices = teams,
                                            selected = "Liverpool"),
                                dateRangeInput("third_page_date_range", "Date Range:",
                                               min = min(dates),
                                               max = max(dates),
                                               start = max(dates) - 365,
                                               end = max(dates)),
                                actionButton("run_forms", "Confirm"),
                                tableOutput("all_time_comparison")
                            ),
                            mainPanel(
                                plotOutput("teams_form_plot")
                            )
                        )
               ),
               
               #### Analiza arbitr?w ####
               
               tabPanel("Referees",
                        
                        
                        tabsetPanel(
                            tabPanel("Statistics",
                                     
                                     sidebarLayout(
                                         sidebarPanel(
                                             
                                             selectInput("fourth_page_select_statistic", "Statistic:",
                                                         choices = c("Fouls (Avg)" = "fouls",
                                                                     "Yellow Cards (Avg)" = "yellowcards",
                                                                     "Red Cards (Avg)" = "redcards",
                                                                     "Refreed Matches" = "refsmatches"),
                                                         selected = "fouls"),
                                             actionButton("run_referees_comparison", "Confirm")
                                             
                                         ),
                                         
                                         
                                         mainPanel(
                                             
                                             
                                             plotOutput("referee_stat")))),
                            
                            
                            
                            
                            tabPanel("Referee-Team",
                                     
                                     tabPanel("Statistics",
                                              
                                              sidebarLayout(
                                                  sidebarPanel(
                                                      selectInput("fourth_page_select_referee", "Referee:",
                                                                  choices = referees,
                                                                  selected = min(referees)),
                                                      selectInput("fourth_page_select_team", "Team:",
                                                                  choices = teams,
                                                                  selected = min(teams)),
                                                      actionButton("run_referee_and_team", "Confirm")
                                                  ),
                                                  
                                                  mainPanel(
                                                      fluidRow(
                                                          plotOutput("referee_team")),
                                                      fluidRow(
                                                          dataTableOutput("referee_team_tab")))
                                                  
                                              )
                                     )
                            ))),
               #### info o skrotach ####
               
               tabPanel("Informations",
                        column(width = 6,
                               h3("Notation"),
                               p("FTHG and HG = Full Time Home Team Goals"), 
                               p("FTAG and AG = Full-Time Away Team Goals"), 
                               p("FTR and Res = Full-Time Result (H=Home Win, D=Draw, A=Away Win)"),
                               p("HTHG = Half Time Home Team Goals"),
                               p("HTAG = Half Time Away Team Goals"),
                               p("HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)"),
                               p("Referee = Match Referee"),
                               p("HS = Home Team Shots"),
                               p("AS = Away Team Shots"),
                               p("HST = Home Team Shots on Target"),
                               p("AST = Away Team Shots on Target"),
                               p("HF = Home Team Fouls Committed"),
                               p("AF = Away Team Fouls Committed"),
                               p("HC = Home Team Corners"),
                               p("AC = Away Team Corners"),
                               p("HY = Home Team Yellow Cards"),
                               p("AY = Away Team Yellow Cards"),
                               p("HR = Home Team Red Cards"),
                               p("AR = Away Team Red Cards")),
                        column(width = 6,
                               h3("Authors"),
                               p("Banaszek Stanisław"),
                               p("Drobina Mateusz"),
                               p("Mika Dominik"),
                               p("Płoszczyca Adrian"),
                               p("Sobkowiak Jakub"),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               h3("Data"),
                               p("The data in the application comes from the seasons from 2008/2009 to 2017/2018."))))
))
