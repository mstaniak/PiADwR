library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Przykładowe ciągłe rozkłady prawdopodobieństwa"),
  tabsetPanel(
    #generujemy panel dla rozkładu normalnego
    tabPanel(title = "Rozkład normalny",
      tabsetPanel(
        tabPanel(
          title = "Wykres",
          numericInput(inputId = "normal_n",
                       label = "Podaj wielkość próby", 
                       value = 1000),
          plotOutput("normal_plot")
        ),
        tabPanel(
          title = "Dane",
          tableOutput("normal_data")
        )
      )
    ),
    #generujemy panel dla rozkładu wykładniczego
    tabPanel(title = "Rozkład wykładniczy",
      tabsetPanel(
        tabPanel(
          title = "wykres",
          numericInput(inputId = "exp_n",
                       label = "Podaj wielkość próby", 
                       value = 1000),
          plotOutput("exp_plot")
        ),
        tabPanel(
          title = "Dane",
          tableOutput("exp_data")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #generujemy dane
  normal_data <- reactive({
    set.seed(17)
    data.frame(id = 1:input[["normal_n"]],
               sample = rnorm(input[["normal_n"]]))
  })
  exp_data <- reactive({
    set.seed(17)
    data.frame(id = 1:input[["exp_n"]],
               sample = rnorm(input[["exp_n"]]))
  })
  #generujemy tabele
  output[["normal_data"]] <- renderTable({
    normal_data()
  })
  output[["exp_data"]] <- renderTable({
    exp_data()
  })
  #generuemy wykresy
  output[["normal_plot"]] <- renderPlot({
    ggplot(normal_data(), aes(x = sample)) +
      geom_density()
  })
  output[["exp_plot"]] <- renderPlot({
    ggplot(exp_data(), aes(x = sample)) +
      geom_density() +
      xlim(0, 5)
  })
}
shinyApp(ui, server)
