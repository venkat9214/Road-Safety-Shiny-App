library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

draw <- vc

shinyServer(function(input, output, session) {
  
## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -73.97, lat = 40.75, zoom = 13)
  })

  # Choose just one vehicle
  drawvalue <- reactive({if (input$vehicle == ''){return(vc)}else{
      t <- filter(vc, VEHICLE.TYPE.CODE.1 == input$vehicle | VEHICLE.TYPE.CODE.2 == input$vehicle)
      return(t)
    }})
 
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    colorBy <- input$color
    sizeBy <- input$size
    draw <- drawvalue()

    colorData <- draw[[colorBy]]
    if (colorBy == "NUMBER.OF.PERSONS.INJURED"|colorBy == "NUMBER.OF.PERSONS.KILLED") {
      pal <- colorBin(heat.colors(7), colorData, 7)} else{
        pal <- colorFactor("Set1", colorData)
      }    
        
    radius <- draw[[sizeBy]] / 9 * 250 + 30
    
    if (input$cluster == TRUE){
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        showGroup('Cluster') %>%
        addCircles(~LONGITUDE, ~LATITUDE, radius=radius, group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addCircleMarkers(~LONGITUDE, ~LATITUDE, radius = 0, group = "Cluster",
                         clusterOptions = markerClusterOptions())%>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }else{
      leafletProxy("map", data = draw) %>%
        clearShapes() %>%
        hideGroup('Cluster') %>%
        addCircles(~LONGITUDE, ~LATITUDE, radius=radius, group = "Circle",
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }
  })

  # Show a popup at the given location
  showvcPopup <- function(eventid, lat, lng) {
    draw <- drawvalue()
    selectedvc <- filter(draw, LATITUDE == lat, LONGITUDE == lng)
    entry <- function(row){
      result <- as.character(tagList(
      tags$h6(row[2], row[3]), 
      tags$strong(HTML(sprintf("%s & %s", row[9], row[10]))), tags$br(),
      sprintf("Vehicles: %s & %s", row[26], row[27]), tags$br(),
      sprintf("Factors: %s & %s", row[20], row[21]), tags$br(),
      sprintf("%s Injuries & %s Deaths", row[12], row[13]), tags$br()))
      return(result)
    }
    content <- apply(selectedvc, 1, entry)
    content <- paste0(content, collapse = "\n")
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = eventid)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showvcPopup(event$id, event$lat, event$lng)
    })
  })

## See Your Neighbourhood ###########################################

  observe({
    zipcodes <- if (is.null(input$boroughs)) character(0) else {
      filter(cleantable, Borough %in% input$boroughs) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })
  
  # When actions is clicked, call popup function for the corresponding latitude and longitude
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.02
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showvcPopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$vctable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        is.null(input$boroughs) | Borough %in% input$boroughs,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
       mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Borough, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
     action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  ####################################
  
  data_ie <- reactive({
    
    if(input$choose == "all"){
      df <- Time_of_day_fatalities %>% group_by(BOROUGH, DATE) %>% summarise(Morning_fatalities = sum(Morning_fatalities), Afternoon_fatalities = sum(Afternoon_fatalities), Evening_fatalities = sum(Evening_fatalities), Night_fatalities = sum(Night_fatalities))
      df <- subset(df, select = c("BOROUGH", "DATE", input$select))
      return (df)
    }
    else{
      
      Tim <- Time_of_fatalities[Time_of_fatalities$VEHICLE.TYPE.CODE.1 == input$choose,]
      Time_of_fatalities1 <- Tim %>% group_by(BOROUGH, DATE) %>% summarise(Morning_fatalities = sum(Morning_fatalities), Afternoon_fatalities = sum(Afternoon_fatalities), Evening_fatalities = sum(Evening_fatalities), Night_fatalities = sum(Night_fatalities))
      Time_of_fatalities1 <- subset(Time_of_fatalities1, select = c("BOROUGH", "DATE", input$select))
      return(Time_of_fatalities1) 
    }
    
    
  })
  
  output$plot_ie <- renderPlot({
    
    g = ggplot(data = data_ie(), aes(x = DATE))
    g + geom_line(aes(group=1, y = data_ie()[,3])) + facet_grid(~BOROUGH) + xlab('Year') + ylab('Number of Fatalities') + ggtitle('Total Fatalities based on time of the day') + theme_economist() + theme(legend.text=element_text(size=8)) + theme(axis.text.x = element_text(size = 8 , angle = 90, hjust = 0)) + theme(axis.text.y = element_text(size = 8 , angle = 0, hjust = 0)) + theme(plot.title = element_text(hjust = 0.5))
    
    
    
    
  })
  
  ####################################

## Most Dangerous Intersections ###########################

  output$toptable <- DT::renderDataTable({
    df <- read.csv('Most_Dangerous_Intersections.csv') %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Borough, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  
  ###########
  predicted1 <- reactive({
    hw <- HoltWinters(myts)
    predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  output$dygraph1 <- renderDygraph({
    dygraph(predicted1(), main = "Predicted Injuries/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
  predicted2 <- reactive({
    hw1 <- HoltWinters(myts1)
    predict(hw1, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  
  output$dygraph2 <- renderDygraph({
    dygraph(predicted2(), main = "Predicted Deaths/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  

})
