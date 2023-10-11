library(shiny)
library(ggplot2)

module_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        title = "Wykres",
        numericInput(inputId = ns("n"),
                     label = "Podaj wielkość próby", 
                     value = 1000),
        plotOutput(ns("plot"))
      ),
      tabPanel(title = "Dane",
               tableOutput(outputId = ns("data"))
      )
    )
  )
}

module_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    #generujemy dane
    data <- reactive({
      set.seed(17)
      data.frame(id = 1:input[["n"]],
                 sample = rnorm(input[["n"]]))
    })
    #generujemy wykres
    output[["plot"]] <- renderPlot({
      ggplot(data(), aes(x = sample)) +
        geom_density()
    })
    #generujemy tabelę
    output[["data"]] <- renderTable({
      data()
    })
  })
}


ui <- fluidPage(
  titlePanel("Przykładowe ciągłe rozkłady prawdopodobieństwa"),
  tabsetPanel(
    #generujemy panel dla rozkładu normalnego
    tabPanel(title = "Rozkład normalny",
             module_UI("norm") 
    ),
    #generujemy panel dla rozkładu wykładniczego
    tabPanel(title = "Rozkład wykładniczy",
             module_UI("exp") 
    )
  )
)

server <- function(input, output, session) {
  module_SERVER("norm")
  module_SERVER("exp")
}


shinyApp(ui, server)

