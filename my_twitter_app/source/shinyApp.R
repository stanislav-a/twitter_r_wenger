library(shiny)
library(leaflet)
library(RColorBrewer)
library(plotly)

get.model.for.zoom<-function(x = c(1, 5, 9, 13, 17), y=c(3000, 800, 100, 15, 1))
{
  lm(y ~ poly(x,4))
}

get.max.radius.by.zoom<-function(zoom, model )
{
  as.numeric(predict(model,data.frame(x=zoom:zoom)))
}

model <- get.model.for.zoom()

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leaflet::leafletOutput("map", width = "100%", height = "100%"),
  shiny::absolutePanel(top = 10, right = 10,
                uiOutput("slider"),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                uiOutput("textBox"),
                checkboxInput("legend", "Show legend", TRUE)
  ),
  shiny::absolutePanel(top = 10, right = "50%",
                       actionButton("getStats", "Get twitter stats!"),
                       plotlyOutput("plot")
  )
)

server <- function(input, output, session) {

  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addTiles() %>%
      setView(lng = 0.1278, lat = 51.5074, zoom = 7)
  })
  
  output$slider <- renderUI({
    sliderInput("inSlider", "Radius, km", min=0, max=10, 
                value=1)
  })
  
  output$textBox <- renderUI(
    {mainPanel(
      textOutput("text1")
    )}
  )
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    click <- input$map_click
    if(!is.null(click))
    {
      leaflet::leafletProxy("map") %>%
        leaflet::clearShapes() %>%
        leaflet::addCircles(lng = click$lng, lat = click$lat, 
                            radius = input$inSlider * 1000, weight = 1, color = "#777777",
                            fillOpacity = 0.7
        )
    }
  })
  
  
  output$scatterplot <- renderPlotly({

  })
  # observeEvent(input$getStats, {
  #   click <- input$map_click
  #   if(is.null(click))
  #     return()
  #   data <- get.stats.by.location(lat = click$lat, long = click$lng, r = input$inSlider, 
  #                                   radius.measure = "km", 
  #                                   n.sample = 100)
  #   output$plot <- plotly::plot_ly(data, labels = ~Categorie, values = ~Percent, type = 'pie') %>%
  #     layout(title = 'Tweets with Wenger hashtags',
  #            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  # })
  
  observe(
    {
      output$text1 <- renderText({ 
        paste("You have selected", as.character(input$map_zoom))
      })
      max.range <- 100
      if(!is.null(input$map_zoom))
        max.range <- round(get.max.radius.by.zoom(input$map_zoom, model ),  digits = 2)
      output$slider <- renderUI({
        sliderInput("inSlider", "Radius, km", min=0, 
                    max= max.range, value = max.range * 0.5)
      })
    }
  )
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)
