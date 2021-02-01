

shinyServer(function(input, output) {

    #### Analiza Sezonu ####
    
    table_for_season <- eventReactive(input$run_table_for_season_button, {
        req(input$second_page_selected_season_input)
        table <- summary_season(input$second_page_selected_season_input)[, 1:8]
        table
    })
    plot_for_stat_season <- eventReactive(input$run_plot_for_stat_button, {
        req(input$second_page_selected_stat_input)
        season_stat_viz(input$second_page_selected_season_input, input$second_page_selected_stat_input)
    })
    
    output$season_table <- renderDataTable({ datatable(table_for_season(), options = list(paging = FALSE, searching = FALSE)) })
    output$season_stat <- renderPlot({ plot_for_stat_season()}, height = 780, width = "auto" )
    
    #### pre - match ####
    
    expected_goals_table <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            showNotification(paste("You have chosen the same home team and away team.
                                   Change one of them and confirm your choice again."),
                             type = "error", duration = 10)
            return()
        }
        table <- as.data.frame(predict_match(input$first_page_select_home_team,
                                             input$first_page_select_away_team)$`Expected goals`)
        colnames(table) <- "Expected goals"
        table
    })
    results_distribution_table <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- as.data.frame(predict_match(input$first_page_select_home_team,
                                             input$first_page_select_away_team)$`Probability`)
        colnames(table) <- "Probability"
        table
    })
    results_distribution_plot <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- head(all_games(input$first_page_select_home_team, input$first_page_select_away_team), input$first_page_last_matches)
        if(any(is.na(as.vector(table)))) {
            return()
        }
        teams_result_barplot(input$first_page_select_home_team,
                             input$first_page_select_away_team,
                             input$first_page_last_matches)
    })
    results_face_to_face_table <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- head(all_games(input$first_page_select_home_team, input$first_page_select_away_team), input$first_page_last_matches)
        if(any(is.na(as.vector(table)))) {
            showNotification(paste("You picked the teams that didn't play in the Premier League 2008-2018 against each other."),
                             type = "error", duration = 15)
            return()
        }
        table
    })
    home_team_last_results <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- head(all_games(input$first_page_select_home_team), input$first_page_last_matches)
        table
    })
    away_team_last_results <- eventReactive(input$run_prediction, {
        req(input$first_page_select_away_team)
        req(input$first_page_select_home_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        table <- head(all_games(input$first_page_select_away_team), input$first_page_last_matches)
        table
    })
    home_team_results_distribution <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        result_barplot(input$first_page_select_home_team,
                       input$first_page_last_matches)
    })
    away_team_results_distribution <- eventReactive(input$run_prediction, {
        req(input$first_page_select_home_team)
        req(input$first_page_select_away_team)
        req(input$first_page_last_matches)
        if(input$first_page_select_home_team == input$first_page_select_away_team) {
            return()
        }
        result_barplot(input$first_page_select_away_team, input$first_page_last_matches)
    })
    
    output$expected_goals <- renderTable({ expected_goals_table() }, rownames = TRUE, colnames = TRUE, width = 300)
    output$results_distribution <- renderTable({ results_distribution_table() }, rownames = TRUE, colnames = TRUE, width = 300)
    output$face_to_face_results_table <- renderDataTable({ datatable(results_face_to_face_table(), options = list(paging = FALSE, searching = FALSE)) })
    output$plot_results_distribution <- renderPlot({ results_distribution_plot()}, height = 350, width = "auto" )
    output$home_team_last_results_table <- renderDataTable({ datatable(home_team_last_results(), options = list(paging = FALSE, searching = FALSE)) })
    output$away_team_last_results_table <- renderDataTable({ datatable(away_team_last_results(), options = list(paging = FALSE, searching = FALSE)) })
    output$home_team_results_distribution_plot <- renderPlot({ home_team_results_distribution()}, height = 300, width = "auto" )
    output$away_team_results_distribution_plot <- renderPlot({ away_team_results_distribution()}, height = 300, width = "auto" )
    
    #### porownanie druzyn ####
    
    teams_form <- eventReactive(input$run_forms, {
        req(input$third_page_select_home_team)
        req(input$third_page_select_away_team)
        req(input$third_page_date_range)
        if(input$third_page_date_range[1] > input$third_page_date_range[2]) {
            showNotification(paste("The selected date range is invalid."),
                             type = "error", duration = 15)
            return()
        }
        if(input$third_page_select_home_team == input$third_page_select_away_team) {
            showNotification(paste("You picked the same team twice."),
                             type = "warning", duration = 15)
            return()
        }
        
        if(dim(period_matches(input$third_page_select_home_team, input$third_page_date_range[1], input$third_page_date_range[2], data))[1] != dim(period_matches(input$third_page_select_away_team, input$third_page_date_range[1], input$third_page_date_range[2]))[1]) {
            showNotification(paste("The teams you selected have played a different number of matches within the given date range."),
                             type = "warning", duration = 15)
            return()
        }
        plot_to_date_team_form(input$third_page_select_home_team, input$third_page_select_away_team, input$third_page_date_range[1], input$third_page_date_range[2])
    })
    
    output$all_time_comparison <- renderTable({ comparison <- cbind(all_time_stats(input$third_page_select_home_team), all_time_stats(input$third_page_select_away_team))
    colnames(comparison) <- c(input$third_page_select_home_team, input$third_page_select_away_team)
    comparison }, rownames = TRUE, colnames = TRUE, width = "100%")
    output$teams_form_plot <- renderPlot({ teams_form() }, width = "auto", height = 700)
    
    #### sedziowie ####
    
    stat_referee <- eventReactive(input$run_referees_comparison, {
        req(input$fourth_page_select_statistic)
        ref_results(input$fourth_page_select_statistic)
    })
    team_referee <- eventReactive(input$run_referee_and_team, {
        req(input$fourth_page_select_referee)
        req(input$fourth_page_select_team)
        if(dim(team_and_ref(input$fourth_page_select_team, input$fourth_page_select_referee))[1] == 0) {
            showNotification(paste(input$fourth_page_select_referee, "has never refereed a", 
                                   input$fourth_page_select_team, "match."),
                             type = "warning", duration = 15)
            return()
        }
        team_and_ref_results(input$fourth_page_select_team, input$fourth_page_select_referee)
    })
    team_referee_tab <- eventReactive(input$run_referee_and_team, {
        req(input$fourth_page_select_referee)
        req(input$fourth_page_select_team)
        if(dim(team_and_ref(input$fourth_page_select_team, input$fourth_page_select_referee))[1] == 0) {
            return()
        }
        table <- team_and_ref(input$fourth_page_select_team, input$fourth_page_select_referee)
        table
    })
    
    output$referee_stat <- renderPlot({ stat_referee() }, width = "auto", height = 700)
    output$referee_team <- renderPlot({ team_referee() }, width = "auto", height = 350)
    output$referee_team_tab <- renderDataTable({ datatable(team_referee_tab(), options = list(paging = TRUE, pageLength = 5, searching = FALSE)) })

})
