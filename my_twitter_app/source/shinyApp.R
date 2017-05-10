library(shiny)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Test Shiny App"),
  
  sidebarPanel(
    #Numeric Inputs
    numericInput("min_val", "Enter Minimum Value", 1993),
    numericInput("max_val", "Enter Maximum Value", 2013),
    #display dynamic UI
    uiOutput("slider")
  ),
  
  mainPanel()
))

s <- shinyServer(function(input, output, session) {
  
  #make dynamic slider
  output$slider <- renderUI({
    sliderInput("inSlider", "Slider", min=input$min_val, max=input$max_val, value=2000)
  })
  
})

shinyApp(ui = ui, server = s)
