library(shiny)
ui <- fluidPage(
  verticalLayout(
    textInput("a","1st str",value = "Hello"),
    textInput("b","2nd str",value = "World!"), 
    actionButton("go", "paste", width = 100),
    br(),
    sidebarPanel(textOutput(outputId = 'ab'))
  )
)
server <-
  function(input,output){ 
    re <- eventReactive(
      input$go, paste(input$a,input$b)
      )
    output$ab <- renderText({re()})
  }
shinyApp(ui, server)

