twitter.data <- get.stats.by.location(r = 30, radius.measure = "km", n.sample = 100)


p <- plot_ly(twitter.data, labels = ~Categorie, values = ~Amount, textposition = 'inside',
             textinfo = 'label+percent',type = 'pie') %>%
  layout(title = paste0('Tweets With #Wenger Hashtags (Recent ', sum(twitter.data$Amount), ' Tweets)'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

shinyApp(
  ui = fluidPage(
    useShinyjs(),
    actionButton("hide", "Hide"),
    p("Text above plot"),
    plotlyOutput("plot"),
    p("Text below plot")
  ),
  server = function(input, output, session) {
    output$plot <- renderPlotly({
      p
    })
    
    observeEvent(input$hide, {
      shinyjs::hide("plot")
      # toggle("plot") if you want to alternate between hiding and showing
    })
  },
  options = list(height = 700)
)
