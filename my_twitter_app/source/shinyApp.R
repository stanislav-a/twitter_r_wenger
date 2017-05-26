library(shiny)
library(leaflet)
library(RColorBrewer)
library(plotly)

split.tags.to.chunks<-function(tags, n =2)
{
  i = 1
  result<-c()
  while(i <= length(tags))
  {
    cur.last.el<-min((i + n - 1), length(tags))
    result<-c(result, paste(tags[i :  cur.last.el], collapse = ";"))
    i = cur.last.el + 1
  }
  paste(collapse ="<br/>", result)
}
create.popup.content<-function(tw.data,
                               yes.tags = c("#WengerIn", "#WengerStay", "#WengerStays",
                                            "#InArseneWeTrust", "#OnlyOneArseneWenger"),
                               no.tags=c("#WengerOut"))
{
  percents <- round(100 * (tw.data$Amount / sum(tw.data$Amount)), digits = 1)
  wenger.in <- paste(sep = "<br/>",
                      paste0("<b><a href='https://twitter.com/search?q=%23wengerin&src=typd'>",percents[1],"%</a></b>"),
                      split.tags.to.chunks(yes.tags),
                      paste(tw.data$Amount[1], "from", sum(tw.data$Amount), " tweets")
  )
  wenger.out <- paste(sep = "<br/>",
                   paste0("<b><a href='https://twitter.com/search?q=%23wengerout&src=typd'>",percents[2],"%</a></b>"),
                   split.tags.to.chunks(no.tags),
                   paste(tw.data$Amount[2], "from", sum(tw.data$Amount), " tweets")
  )
  uncl <- paste(sep = "<br/>",
                     paste0("<b><a href='https://twitter.com/search?q=%23wengerin%23wengerout&src=typd'>",percents[3],"%</a></b>"),
#                     paste(no.tags, collapse = ";"),
                     paste(tw.data$Amount[3], "from", sum(tw.data$Amount), " tweets")
  )
  c(wenger.in, wenger.out, uncl)
}

get.model.for.zoom<-function(x = c(1, 5, 9, 13, 17), y=c(3000, 800, 100, 15, 1))
{
  lm(y ~ poly(x,4))
}

get.max.radius.by.zoom<-function(zoom, model )
{
  as.numeric(predict(model,data.frame(x=zoom:zoom)))
}

get.stats.centers<-function(bounds)
{
  map.center <- list()
  map.center$lat <- 0.5 * (bounds$north + bounds$south)
  map.center$lng <- 0.5 * (bounds$west + bounds$east) 
  
  stats.loc <- list()
  stats.loc$center <- map.center
  stats.loc$left <- list()
  stats.loc$left$lat <- map.center$lat
  stats.loc$left$lng <- (bounds$west + 3 * map.center$lng) /4
  stats.loc$right <- list()
  stats.loc$right$lat <- map.center$lat
  stats.loc$right$lng <- ( bounds$east + 3 *map.center$lng) /4
  data.frame(long = c(stats.loc$left$lng, map.center$lng, stats.loc$right$lng),
                         lat = c(stats.loc$left$lat, map.center$lat, stats.loc$right$lat))
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
                       actionButton("getStats", "Get twitter stats!")
  )
)

server <- function(input, output, session) {
# 
#   
#   # This reactive expression represents the palette function,
#   # which changes as the user makes selections in UI.
#   colorpal <- reactive({
#     colorNumeric(input$colors, quakes$mag)
#   })
  
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
        leaflet::addCircles(lng = click$lng, lat = click$lat, layerId = "Pos",
                            radius = input$inSlider * 1000, weight = 1, color = "#777777",
                            fillOpacity = 0.7
        )
    }
  })
  
  observeEvent(input$getStats, {
    click <- input$map_click
    if(is.null(click))
      return()
    tw.data <- get.stats.by.location(lat = click$lat, long = click$lng, r = input$inSlider,
                                    radius.measure = "km",
                                    n.sample = 100)
    content <- create.popup.content(tw.data)
    bounds <- input$map_bounds#north, east, south, and west
    stats.loc <- get.stats.centers(bounds)
    stats.data <- cbind(stats.loc, tw.data)
    pal <- colorFactor(c("orange", "red", "navy"), domain = stats.data$Categorie)
    stats.data$r <- sqrt((stats.data$Amount / sum(stats.data$Amount))) * (input$inSlider * 700)
    leaflet::leafletProxy("map") %>%
      leaflet::clearShapes() %>%
      #leaflet::removeShape(layerId = "Stats") %>%
      leaflet::addCircles(lat = stats.data$lat, lng = stats.data$long,
                          radius = stats.data$r, weight = 1, color = pal(stats.data$Categorie),
                          fillOpacity = 0.7,popup = paste(stats.data$Categorie)
        
      
            # leaflet::addCircles(layerId = "Stats",
            #               radius = ~10^Amount/10, weight = 1, color = "#777777",
            #               fillColor = ~Amount, fillOpacity = 0.7, popup = ~paste(Categorie)
      )%>%
      addMarkers(
        lat = stats.data$lat, lng = stats.data$long,
        label = stats.data$Categorie,
        labelOptions = labelOptions(noHide = T, textsize = "15px")) %>%
      addPopups(lat = stats.data$lat, lng = stats.data$long, content,
                options = popupOptions(keepInView = T)
      )
    
  })
  
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
  # observe({
  #   proxy <- leafletProxy("map", data = quakes)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #     )
  #   }
  # })
}

shinyApp(ui, server)
