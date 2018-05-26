library(shiny)

ui <- shinyUI(
  fluidPage(
    title = "Teste",
    sliderInput("INP", "Valor", 0, 1, 0.5),
    verbatimTextOutput("aux"),
    uiOutput("coringa")
  )
)

# server.R
server <- shinyServer({
  function(input,output) {
    
    output$aux <- renderPrint({
      input$INP
    })
    
    output$coringa <- renderUI({
      if (input$INP > 0.5) {
        plotOutput("PLOT")
      } else {
        h1("Coloca o valor certo!")  
      }
    })
    
    output$PLOT <- renderPlot({
      hist(rbinom(1000, 10, input$INP))
    })
  }
})

shinyApp(ui = ui,server = server)
