library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggforce)
library(ggplot2)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
data <- cleanData[sample.int(nrow(cleanData), 10000),]
plotData <- cleanData

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_blank()
    #plot.title=element_text(size=14, face="bold")
  )
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -3.74, lat = 40.46, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  #zipsInBounds <- reactive({
    #if (is.null(input$map_bounds))
      #return(zipdata[FALSE,])
    #bounds <- input$map_bounds
    #latRng <- range(bounds$north, bounds$south)
    #lngRng <- range(bounds$east, bounds$west)

    #subset(zipdata,
      #latitude >= latRng[1] & latitude <= latRng[2] &
        #longitude >= lngRng[1] & longitude <= lngRng[2])
  #})

  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  #output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    #if (nrow(zipsInBounds()) == 0)
      #return(NULL)

    #hist(zipsInBounds()$centile,
      #breaks = centileBreaks,
      #main = "SuperZIP score (visible zips)",
      #xlab = "Percentile",
      #xlim = range(allzips$centile),
      #col = '#00DD00',
      #border = 'white')
  #})

  #output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    #if (nrow(zipsInBounds()) == 0)
      #return(NULL)

    #print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  #})

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
   observe({
    df <- cleanData %>%
      filter(
        Date >= input$minDate,
        Date <= input$maxDate,
        is.null(input$countryFilter) | cleanData$srccountry %in% input$countryFilter,
        is.null(input$serviceFilter) | cleanData$service %in% input$serviceFilter
      ) %>%
    mutate()
  # 
  #   # if (colorBy == "superzip") {
  #   #   # Color and palette are treated specially in the "superzip" case, because
  #   #   # the values are categorical instead of continuous.
  #   #   colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
  #   #   pal <- colorFactor("viridis", colorData)
  #   # } else {
  #     colorData <- zipdata[[colorBy]]
  #     pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
  #   # }
  # 
  #   # if (sizeBy == "superzip") {
  #   #    # Radius is treated specially in the "superzip" case.
  #   #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
  #   # } else {
  #     radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
  #   # }
  # 
  #   leafletProxy("map", data = zipdata) %>%
  #     clearShapes() %>%
  #     addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
  #       stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
  #     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
  #       layerId="colorLegend")
  })

  # Show a popup at the given location
  showCountryPopup <- function(country, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$strong(HTML(sprintf("%s, %s %s",
        selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Country: %s", dollar(selectedZip$income * 1000)), tags$br(),
      sprintf("Number of Connections: %s%", as.integer(selectedZip$college)), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showCountryPopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  output$topservices <- renderPlot({
    data <- cleanData %>% group_by(service) %>%
        summarise(count= n())
    data <- data[order(data$count, decreasing = TRUE),]
    total <- sum(data$count)
    top <- head(data, 5)
    toptotal <- sum(top$count)
    result <- rbind(top, c("Other services", total-toptotal))
    #generate plot
    pie(as.integer(result$count), labels=result$service, main="Attacks by service")
  })
  
  output$topcountries <- renderPlot({
    data2 <- cleanData %>% group_by(srccountry) %>%
      summarise(count= n())
    data2 <- data2[order(data2$count, decreasing = TRUE),]
    total2 <- sum(data2$count)
    top2 <- head(data2, 5)
    toptotal2 <- sum(top2$count)
    result2 <- rbind(top2, c("Other countries", total2-toptotal2))
    #generate plot
    pie(as.integer(result2$count), labels=result2$srccountry, main="Attacks by country")
  })
  
  output$cph <- renderPlot({
    df <- cleanData %>%
      filter(
        Date >= input$minDate,
        Date <= input$maxDate,
        is.null(input$countryFilter) | cleanData$srccountry %in% input$countryFilter,
        is.null(input$serviceFilter) | cleanData$service %in% input$serviceFilter
      ) %>%
      mutate()
    hours <- list(substr(df$Time, 0, 2))
    hourCounts <- table(hours)
    
    if(!is.null(input$serviceFilter)){
      services <- " selected services "
    }
    else services <- " "
    if(!is.null(input$countryFilter)){
      countries <- " from selected countries"
    }
    else countries <- " "
        
    title <- paste("Number of", services, "connections per hour", countries, sep="")
    
    barplot(hourCounts, main=title, xlab="Hour of day")
  })
  
  output$data <- DT::renderDataTable({
    df <- cleanData %>%
      filter(
        Date >= input$minDate,
        Date <= input$maxDate,
        is.null(input$countryFilter) | cleanData$srccountry %in% input$countryFilter,
        is.null(input$serviceFilter) | cleanData$service %in% input$serviceFilter
      ) %>%
      mutate()
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}