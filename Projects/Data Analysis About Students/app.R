library(shiny)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(shinythemes)
library(stringr)
library(data.table)

mat <- fread("student-mat.csv")
port <- fread("student-por.csv")

model_mat <- glm(G3 ~ factor(school) + factor(sex) + age + factor(address) + factor(famsize) + factor(Pstatus) + factor(Medu) 
                 + factor(Fedu) + factor(Mjob) + factor(Fjob) + factor(reason) + factor(guardian) + factor(traveltime) + factor(studytime) 
                 + factor(failures) + factor(schoolsup) + factor(famsup) + factor(paid) + factor(activities) + factor(nursery) + factor(higher)
                 + factor(internet) + factor(romantic) + factor(famrel) + factor(freetime) + factor(goout) + factor(Dalc) + factor(Walc) 
                 + factor(health) + absences, data = mat)

model_por <- glm(G3 ~ factor(school) + factor(sex) + age + factor(address) + factor(famsize) + factor(Pstatus) + factor(Medu) 
                 + factor(Fedu) + factor(Mjob) + factor(Fjob) + factor(reason) + factor(guardian) + factor(traveltime) + factor(studytime) 
                 + factor(failures) + factor(schoolsup) + factor(famsup) + factor(paid) + factor(activities) + factor(nursery) + factor(higher)
                 + factor(internet) + factor(romantic) + factor(famrel) + factor(freetime) + factor(goout) + factor(Dalc) + factor(Walc) 
                 + factor(health) + absences, data = port)

setkeyv(mat, c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob",
               "reason", "nursery", "internet", "traveltime", "romantic", "guardian", "famrel", "studytime",
               "schoolsup", "famsup", "activities", "higher", "freetime", "goout",  "Dalc", "Walc", "health"))
setkeyv(port, c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob",
                "reason", "nursery", "internet", "traveltime", "romantic", "guardian", "famrel", "studytime",
                "schoolsup", "famsup", "activities", "higher", "freetime", "goout",  "Dalc", "Walc", "health"))

both <- mat[port, nomatch = 0]
together <- merge(mat, port, all = TRUE)

setnames(together, 
         c("G1.x", "G2.x", "G3.x", "failures.x", "paid.x", "absences.x", "G1.y", "G2.y", "G3.y", "failures.y", "paid.y", "absences.y"), 
         c("math.G1", "math.G2", "math.G3", "math.failures", "math.paid", "math.absences", "port.G1", "port.G2", "port.G3", "port.failures",
           "port.paid", "port.absences"))

info <- read.csv("column_info.csv")
columns <- colnames(mat)

ui <- fluidPage(
    theme = shinytheme("sandstone"),
    titlePanel("Student Performance Data Set"),
    tabsetPanel(
        tabPanel("Data",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "school", 
                             label = "Choose school:", 
                             choices = c("GP", "MS", "Both")
                         ),
                         pickerInput(
                             inputId = "check_cols",
                             label = "Select columns", 
                             choices = colnames(together),
                             selected = c("sex", "age", "address"),
                             options = list(
                                 `actions-box` = TRUE), 
                             multiple = TRUE
                         ),
                         tableOutput("data_cols_info")
                     ),
                     mainPanel(
                         dataTableOutput("table")
                     )
                 )
        ),
        tabPanel("Plots",
                 tabsetPanel(
                     tabPanel("Bar",
                              sidebarLayout(
                                  sidebarPanel(
                                      radioGroupButtons(
                                          inputId = "barplot_subject",
                                          label = "Choose subject", 
                                          choices = c("Math", "Portuguese"),
                                          justified = TRUE
                                      ),
                                      pickerInput(
                                          inputId = "barplot_x",
                                          label = "X", 
                                          choices = columns[1:30],
                                          selected = "freetime"
                                      ),
                                      htmlOutput("barplot_x_info"),
                                      pickerInput(
                                          inputId = "barplot_fill",
                                          label = "Fill", 
                                          choices = columns[1:30],
                                          selected = "studytime"
                                      ),
                                      htmlOutput("barplot_fill_info"),
                                      switchInput(
                                          label = "Split",
                                          inputId = "barplot_split_switch"
                                      ),
                                      conditionalPanel("input.barplot_split_switch",
                                                       pickerInput(
                                                           inputId = "barplot_split",
                                                           label = "Split", 
                                                           choices = columns[1:30],
                                                           selected = "sex"
                                                       ),
                                                       htmlOutput("barplot_split_info")
                                      )
                                  ),
                                  mainPanel(
                                      plotOutput("barplot")
                                  )
                              )
                     ),
                     tabPanel("Hist+Box",
                              sidebarLayout(
                                  sidebarPanel(
                                      radioGroupButtons(
                                          inputId = "histbox_subject",
                                          label = "Choose subject", 
                                          choices = c("Math", "Portuguese"),
                                          justified = TRUE
                                      ),
                                      radioGroupButtons(
                                          inputId = "histbox_grade",
                                          label = "Choose grade", 
                                          choices = c("G1", "G2", "G3"),
                                          justified = TRUE
                                      ),
                                      radioGroupButtons(
                                          inputId = "histogram_type",
                                          label = "Choose type", 
                                          choices = c("density", "count"),
                                          justified = TRUE
                                      )
                                  ),
                                  mainPanel(
                                      splitLayout(cellWidths = c("50%", "50%"), plotOutput("histogram"), plotOutput("boxplot"))
                                  )
                              )
                     ),
                     tabPanel("Point",
                              sidebarLayout(
                                  sidebarPanel(
                                      radioGroupButtons(
                                          inputId = "point_subject",
                                          label = "Choose subject", 
                                          choices = c("Math", "Portuguese"),
                                          justified = TRUE
                                      ),
                                      pickerInput(
                                          inputId = "point_x",
                                          label = "X", 
                                          choices = columns,
                                          selected = "G3"
                                      ),
                                      htmlOutput("point_x_info"),
                                      pickerInput(
                                          inputId = "point_y",
                                          label = "Y", 
                                          choices = columns,
                                          selected = "absences"
                                      ),
                                      htmlOutput("point_y_info"),
                                      pickerInput(
                                          inputId = "point_color",
                                          label = "Color", 
                                          choices = columns,
                                          selected = "sex"
                                      ),
                                      htmlOutput("point_color_info"),
                                      pickerInput(
                                          inputId = "point_size",
                                          label = "Size", 
                                          choices = columns,
                                          selected = "age"
                                      ),
                                      htmlOutput("point_size_info"),
                                      switchInput(
                                          label = "Jitter",
                                          inputId = "point_jitter_switch"
                                      )
                                  ),
                                  mainPanel(
                                      plotOutput("point")
                                  )
                              )
                     )
                 )
        ),
        tabPanel("Predictions",
                 fluidRow(
                     column(2,
                            pickerInput(inputId = "pred_school", label = "School", choices = sort(unique(mat$school)), selected = "GP"),
                            pickerInput(inputId = "pred_sex", label = "Sex", choices = sort(unique(mat$sex)), selected = "F"),
                            pickerInput(inputId = "pred_age", label = "Age", choices = 15:22, selected = 18),
                            pickerInput(inputId = "pred_address", label = "Address", choices = sort(unique(mat$address)), selected = "U"),
                            pickerInput(inputId = "pred_famsize", label = "Family Size", choices = sort(unique(mat$famsize)), selected = "GT3"),
                            pickerInput(inputId = "pred_Pstatus", label = "Parent's Cohabitation Status", choices = sort(unique(mat$Pstatus)), selected = "A")
                     ),
                     column(2,
                            pickerInput(inputId = "pred_Medu", label = "Mother's Education", choices = sort(unique(mat$Medu)), selected = 0),
                            pickerInput(inputId = "pred_Fedu", label = "Father's Education", choices = sort(unique(mat$Fedu)), selected = 0),
                            pickerInput(inputId = "pred_Mjob", label = "Mother's Job", choices = sort(unique(mat$Mjob)), selected = "at_home"),
                            pickerInput(inputId = "pred_Fjob", label = "Father's Job", choices = sort(unique(mat$Fjob)), selected = "teacher"),
                            pickerInput(inputId = "pred_reason", label = "Reason", choices = sort(unique(mat$reason)), selected = "course"),
                            pickerInput(inputId = "pred_schoolsup", label = "School Educational Support", choices = sort(unique(mat$schoolsup)), selected = "yes")
                     ),
                     column(2,
                            pickerInput(inputId = "pred_traveltime", label = "Travel Time", choices = sort(unique(mat$traveltime)), selected = 1),
                            pickerInput(inputId = "pred_studytime", label = "Study Time", choices = sort(unique(mat$studytime)), selected = 1),
                            pickerInput(inputId = "pred_failures", label = "Failures", choices = sort(unique(mat$failures)), selected = 0),
                            pickerInput(inputId = "pred_paid", label = "Paid", choices = sort(unique(mat$paid)), selected = "no"),
                            pickerInput(inputId = "pred_guardian", label = "Guardian", choices = sort(unique(mat$guardian)), selected = "mother"),
                            pickerInput(inputId = "pred_famsup", label = "Family Educational Support", choices = sort(unique(mat$famsup)), selected = "no")
                     ),
                     column(2,
                            pickerInput(inputId = "pred_activities", label = "Activities", choices = sort(unique(mat$activities)), selected = "no"),
                            pickerInput(inputId = "pred_nursery", label = "Nursery", choices = sort(unique(mat$nursery)), selected = "yes"),
                            pickerInput(inputId = "pred_higher", label = "Want Higher Education", choices = sort(unique(mat$higher)), selected = "yes"),
                            pickerInput(inputId = "pred_romantic", label = "Romantic Relationships", choices = sort(unique(mat$romantic)), selected = "no"),
                            pickerInput(inputId = "pred_famrel", label = "Family Relationships", choices = sort(unique(mat$famrel)), selected = 1),
                            pickerInput(inputId = "pred_Dalc", label = "Workday Alcohol Consumption", choices = sort(unique(mat$Dalc)), selected = 1)
                     ),
                     column(2,
                            pickerInput(inputId = "pred_freetime", label = "Free Time", choices = sort(unique(mat$freetime)), selected = 1),
                            pickerInput(inputId = "pred_goout", label = "Going Out", choices = sort(unique(mat$goout)), selected = 1),
                            pickerInput(inputId = "pred_internet", label = "Internet", choices = sort(unique(mat$internet)), selected = "no"),
                            pickerInput(inputId = "pred_health", label = "Health Status", choices = sort(unique(mat$health)), selected = 1),
                            pickerInput(inputId = "pred_absences", label = "Number Of Absences", choices = 0:93, selected = 0),
                            pickerInput(inputId = "pred_Walc", label = "Weekend Alcohol Consumption", choices = sort(unique(mat$Walc)), selected = 1)
                     )
                 ),
                 switchInput(
                     label = "Show Parameters",
                     inputId = "show_pred_param_switch",
                     value = FALSE
                 ),
                 conditionalPanel("input.show_pred_param_switch",
                                  tableOutput("pred_param1"),
                                  tableOutput("pred_param2"),
                                  tableOutput("pred_param3")
                 ),
                 actionButton(
                     inputId = "button_pred",
                     label = "Predict Grade"
                 ),
                 textOutput("pred_output_mat"),
                 textOutput("pred_output_por")
        ),
        tabPanel("About", uiOutput("about"))
    )
)

server <- function(input, output) {
    # Data
    output$table <- renderDataTable(
        together %>%
            { if (input$school != "Both") filter(., school == input$school) else . } %>%
            select(input$check_cols)
    )
    output$data_cols_info <- renderUI({ HTML(info$column_info[is.element(info$column_name, input$check_cols)]) })
    # Plots
    ## Bar
    output$barplot <- renderPlot(
        ggplot(group_by(if (input$barplot_subject == "Math") { mat } else { port }, input$barplot_x), 
               aes_string(x = input$barplot_x, fill = paste("factor(", input$barplot_fill, ")", sep = ""))) +
            geom_bar(color = "black") + 
            facet_wrap(if (input$barplot_split_switch == "TRUE") { reformulate(input$barplot_split) } else { NULL }) +
            ggtitle(paste(toupper(input$barplot_subject), 'COURSE'), if (input$barplot_split_switch == "TRUE") {toupper(input$barplot_split)} else { "" }) + 
            xlab(toupper(input$barplot_x)) + 
            ylab(toupper(input$barplot_y)) + 
            labs(fill = toupper(input$barplot_fill)) +
            theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    )
    output$barplot_x_info <- renderUI({ HTML(info$column_info[info$column_name == input$barplot_x]) })
    output$barplot_fill_info <- renderUI({ HTML(info$column_info[info$column_name == input$barplot_fill]) })
    output$barplot_split_info <- renderUI({ HTML(info$column_info[info$column_name == input$barplot_split]) })
    
    ## Hist+Box
    output$histogram <- renderPlot(
        ggplot(if (input$histbox_subject == "Math") { mat } else { port }, 
               aes_string(input$histbox_grade, paste("..", input$histogram_type, "..", sep = ""))) + 
            geom_histogram(bins = 10, fill = 'white', color = 'black') +
            ggtitle('HISTOGRAM', paste(toupper(input$histbox_subject), 'COURSE')) + 
            xlab(toupper(input$histbox_grade)) + 
            ylab(toupper(input$histogram_type)) + 
            theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
    )
    output$boxplot <- renderPlot(
        ggplot(if (input$histbox_subject == "Math") { mat } else { port }, aes_string(y = input$histbox_grade)) +
            stat_boxplot(geom = 'errorbar', width = 0.5) + 
            geom_boxplot() + ggtitle('BOXPLOT', paste(toupper(input$histbox_subject), 'COURSE')) + 
            xlab('') + 
            ylab(toupper(input$histbox_grade)) + 
            theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    )
    
    ## Point
    output$point <- renderPlot(
        ggplot(if (input$point_subject == "Math") { mat } else { port }, 
               aes_string(x = input$point_x, y = input$point_y, color = input$point_color, size = input$point_size)) +
            geom_point(alpha = 0.4, position = if (input$point_jitter_switch) { "jitter" } else { "identity" }) + 
            ggtitle(paste(toupper(input$point_y), 'BY', toupper(input$point_size), 'AND', toupper(input$point_color)), paste(toupper(input$point_subject), 'COURSE')) + 
            xlab(toupper(input$point_x)) + 
            ylab(toupper(input$point_y)) + 
            labs(color = toupper(input$point_color), size = toupper(input$point_size)) +
            theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))  
    )
    output$point_x_info <- renderUI({ HTML(info$column_info[info$column_name == input$point_x]) })
    output$point_y_info <- renderUI({ HTML(info$column_info[info$column_name == input$point_y]) })
    output$point_color_info <- renderUI({ HTML(info$column_info[info$column_name == input$point_color]) })
    output$point_size_info <- renderUI({ HTML(info$column_info[info$column_name == input$point_size]) })
    
    # Predictions
    prediction_parameters <- reactive({ data.table(school = input$pred_school,
                                                  sex = input$pred_sex,
                                                  age = as.numeric(input$pred_age),
                                                  address = input$pred_address,
                                                  famsize = input$pred_famsize,
                                                  Pstatus = input$pred_Pstatus,
                                                  Medu = input$pred_Medu,
                                                  Fedu = input$pred_Fedu,
                                                  Mjob = input$pred_Mjob,
                                                  Fjob = input$pred_Fjob,
                                                  reason = input$pred_reason,
                                                  guardian = input$pred_guardian,
                                                  traveltime = input$pred_traveltime,
                                                  studytime = input$pred_studytime,
                                                  failures = input$pred_failures,
                                                  schoolsup = input$pred_schoolsup,
                                                  famsup = input$pred_famsup,
                                                  paid = input$pred_paid,
                                                  activities = input$pred_activities,
                                                  nursery = input$pred_nursery,
                                                  higher = input$pred_higher,
                                                  internet = input$pred_internet,
                                                  romantic = input$pred_romantic,
                                                  famrel = input$pred_famrel,
                                                  freetime = input$pred_freetime,
                                                  goout = input$pred_goout,
                                                  Dalc = input$pred_Dalc,
                                                  Walc = input$pred_Walc,
                                                  health = input$pred_health,
                                                  absences = as.numeric(input$pred_absences)) })
    output$pred_param1 <- renderTable({ prediction_parameters()[,1:10] })
    output$pred_param2 <- renderTable({ prediction_parameters()[,11:20] })
    output$pred_param3 <- renderTable({ prediction_parameters()[,21:30] })
    
    prediction_output_mat <- eventReactive(input$button_pred, {
        paste("Predicted Math Grade:", round(predict(model_mat, newdata = prediction_parameters())))
    })
    prediction_output_por <- eventReactive(input$button_pred, {
        paste("Predicted Portuguese Grade:", round(predict(model_por, newdata = prediction_parameters())))
    })
    output$pred_output_mat <- renderText({
        prediction_output_mat()
    })
    output$pred_output_por <- renderText({
        prediction_output_por()
    })
    
    # About
    output$about <- renderUI({
        includeHTML(path = "about.html")
    })
}

shinyApp(ui, server)