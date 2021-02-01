library(shinydashboard)

shinyServer(function(input, output, session) {
  # National Statistics ----
  national_statisticks_table <- reactive({
    get_national_statisticks_table(
      data = sum_of_var_Dt, 
      filter = input$filter,
      sort = eval(parse(text =input$sort)), 
      countries = input$countries)
    })

  titles_plot_national_statisticks <- reactive({
    list('ncases' = 'Number of cases',
         'nkill' = 'Number of killed',
         'nkillter' = 'Number of terrorists killed',
         'nwound' = 'Number of wounded',
         'propvalue' = 'Value of property damage'
         )
    })
  
  
  plot_national_statisticks_table <- eventReactive(
    eventExpr = input$go_button_ns,
    {ggplot(
      data = national_statisticks_table()[1:input$show ], 
      mapping = aes(x = reorder(country, eval(parse(text = input$filter))),
                    y = eval(parse(text = input$filter)))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("") +
      ylab("") +
      theme_gray(
        base_size = 18, 
        base_family = "Times") +
      theme(axis.text.x = element_text(angle = 0)) +
      ggtitle(titles_plot_national_statisticks()[input$filter])}
    )
  
  output$plot_national_statisticks_table <- renderPlot({
    plot_national_statisticks_table()
    })
  
  table_national_statisticks_table <- eventReactive(
    eventExpr = input$go_button_ns,
    {national_statisticks_table()[1:input$show ]}
    )
  
  output$table_national_statisticks_table <- renderTable({
    table_national_statisticks_table()
    })
  
  # correlations ----
  plot_correlations <- eventReactive(
    eventExpr = input$go_button_cor,
    {ggplot(
      data = dt[country_txt %in% input$correlations_countries,],
      mapping = aes(x = eval(parse(text = input$correlations_explanatory)), 
                    y = eval(parse(text = input$correlations_dependent)),
                    color = eval(parse(text = input$correlations_group_by)))) +
        geom_point() +
        geom_smooth(method = input$correlations_predict_line) +
        xlab(correlated_variables_titles[input$correlations_explanatory]) +
        ylab(correlated_variables_titles[input$correlations_dependent]) +
        #ggtitle("correlations") +
        theme(
          plot.title = element_text(size = 20, 
                                    face = "bold", 
                                    margin = margin(10, 0, 10, 0)),
          legend.position = "bottom",
          legend.title = element_blank()
          )
      }
    )
  
  output$plot_correlations <- renderPlot({
    plot_correlations()
    })
  
  # attack type ----
  table_att_per_country <- eventReactive(
    eventExpr = input$go_button_att,
    {data <- dt[country_txt %in% input$countries_att, 
                .N, 
                by = attacktype1_txt][order(N, decreasing =eval(parse(text =input$sort_att)))]
    setnames(data,  c("attacktype1_txt", "N"), c("attack", "count"))
    data}
    )
  
  output$table_att_per_country <- renderTable({
    table_att_per_country()
    })
  
  table_att_per_region <- eventReactive(
    eventExpr = input$go_button_att_r,
    {data <- dt[region_txt %in% input$regions_att, 
                .N, 
                by = attacktype1_txt][order(N, decreasing =eval(parse(text =input$sort_att_r)))]
    setnames(data, c("attacktype1_txt", "N"), c("attack", "count"))
    data}
    )
  
  output$table_att_per_region <- renderTable({
    table_att_per_region()
    })
  
  
  plot_guns_per_country <- eventReactive(
    eventExpr = input$go_button_att,
    {ggplot(table_att_per_country(), 
            aes(x = reorder(attack, sorting(input$sort_att, count)),
                y = count) ) + 
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_gray(
          base_size = 18, 
          base_family = "Times") +
        theme(
          axis.text.x = element_text(angle = 0),
          plot.title = element_text(face="bold")) +
        #xlab("Type") + 
        xlab("") +
        ggtitle("Amount of attacks in country") + 
        ylab("Amount of attacks")}
    )
  
  output$plot_guns_per_country <- renderPlot({
    plot_guns_per_country()
    })
  
  plot_guns_per_region <- eventReactive(
    eventExpr = input$go_button_att_r,
    {ggplot(table_att_per_region(), 
            aes(x = reorder(attack, sorting(input$sort_att_r, count)),
                y = count)) + 
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_gray(
          base_size = 18, 
          base_family = "Times") +
        theme(
          axis.text.x = element_text(angle = 0),
          plot.title = element_text(face="bold")) +
        #xlab("Type") + 
        xlab("") +
        ggtitle("Amount of attacks in region") + 
        ylab("Amount of attacks")
      }
    )
  
  output$plot_guns_per_region <- renderPlot({
    plot_guns_per_region()
    })
  # maps ----
  chosen_countries_years <- reactive({
    dt[country_txt %in% input$countries_map & iyear %between% input$years_map, 
       .(latitude,longitude,summary,attacktype1_txt)]
    })
  
  observeEvent(
    eventExpr = input$show_map,
    {
      if(nrow(chosen_countries_years()) == 0) {
        showNotification(
          "We couldn't find any data for chosen countries and range of years.", 
          duration = 10)
        }
      })
  
  create_maps <- eventReactive(
    eventExpr = input$show_map,
    {
      if(nrow(chosen_countries_years()) == 0) return()
      if(input$map_type == "Markers") {
        leaflet(chosen_countries_years()) %>%
          addTiles() %>%
          addMarkers(
            lat = ~latitude, 
            lng = ~longitude, 
            popup = ~summary, 
            clusterOptions=markerClusterOptions(),
            label = ~attacktype1_txt)
      } else {
        leaflet(chosen_countries_years()) %>%
          addTiles() %>%
          addCircleMarkers(
            lat = ~latitude, 
            lng = ~longitude, 
            popup = ~summary, 
            fill = TRUE, 
            fillOpacity = 0.8, 
            color = ~attacktype_palette(attacktype1_txt), 
            radius = 3, 
            label = ~attacktype1_txt) %>%
          addLegend(
            pal = attacktype_palette, 
            position = "bottomright", 
            values = ~attacktype1_txt, 
            title = "Attack type")
      }
      }
    )
  
  output$map <- renderLeaflet({
    create_maps()
    })
  
  # years ----
  output$PlotRegions <- renderPlot({
    ggplot(
      regionYear[, 
                 .(sum_nattacks = sum(nattacks)), 
                 by = .(region, year)][year %between% input$years & region %in% input$Regions,],
      aes(
        x = region,
        y = sum_nattacks, 
        fill = region)) +
      geom_bar(
        stat = "identity", 
        show.legend = FALSE ) +
      coord_flip() +
      ggtitle("Total Amount of Attacks depending on World Regions") +
      ylab("Number of Attacks") +
      xlab("") +
      scale_fill_manual(values = regionCol) +
      theme(
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face="bold"))
    })
  
  output$PlotYear <- renderPlot({
    ggplot(
      regionYear[year %between% input$years & region %in% input$Regions,],
      aes(
        x = year, 
        y = nattacks, 
        color = region)) +
      geom_line() +
      geom_point() +
      xlab("Year") + 
      ggtitle("Terrorist Attacks in World Regions") + 
      ylab("Attack number") +
      scale_color_manual(values = regionCol) 
    # theme(legend.justification = c(0,1), legend.position = c(0,1), legend.title = element_blank(),
    #       plot.title = element_text(face="bold")) +
    # guides(col = guide_legend(ncol = 3))
    })
  }
  )