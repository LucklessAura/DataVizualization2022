library(shiny)
library(leaflet)
library(geojsonio)
library(sp)
library(rjson)
library(tidyr)
library(tidyverse)
library(lubridate)
library(shinyjs)
source(file = "./Data.R")
source(file = "./Misc.R")


choices <- NULL
currentLabel <- NULL
fullWorldPolygonData <- geojson_read("./countries.geojson",what = "sp")
polygonData <- fullWorldPolygonData
colorToUse <- NULL
periodChoices <- list("2000","2010","2000 and 2010")
selectedCountries <- list()
lastEventSecret <- -1.0


swapColors <- function()
{
  colorToUse <<- colorRamp(colorIntervals[[sample(1:length(colorIntervals), 1)]], interpolate="linear")
}


ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("some title"),
  
  fluidRow(
    column(12,radioButtons("interactiontype",label = "Map Use",selected = 3,choices = list("Map Data" = 1, "Map Select" = 2, "Country Select" = 3))),
    column(12,selectizeInput("periodSelect",h3("Select data period"),choices = periodChoices)),
    column(4,selectizeInput("activitySelect",h3("Select the activity"),choices = NULL)),
    column(4,selectizeInput("genderSelect",h3("Select the gender"),choices = NULL)),
    column(4,selectizeInput("ageIntervalSelect",h3("Select the age interval"),choices = NULL)),
    column(12,leafletOutput("map",width = "100%", height = 800)),
    column(12,conditionalPanel(condition = "input.interactiontype == 2",plotOutput("countryComparisonGraphs"))), # to implement?
    column(12,conditionalPanel(condition = "input.interactiontype == 3",plotOutput("singleCountryComp"))) # to implement?
  )
  
)

server <- function(input, output, session) {
  
  observe({
    DataReader(input$periodSelect)  
    polygonData <<- fullWorldPolygonData[fullWorldPolygonData@data$ISO_A3 %in% UsedCountriesList,]
    updateSelectizeInput(session = session,"activitySelect",choices = UsedActivities,server = TRUE,
                         options = options(closeAfterSelect = TRUE,placeholder = 'Activity', valueField = 'value', labelField = 'label'))
    
    updateSelectizeInput(session = session,"genderSelect",choices = UsedSexes,server = TRUE,
                         options = options(closeAfterSelect = TRUE,placeholder = 'Sex', valueField = 'value', labelField = 'label'))
    
    updateSelectizeInput(session = session,"ageIntervalSelect",choices = UsedAgeIntervals,server = TRUE,
                         options = options(closeAfterSelect = TRUE,placeholder = 'Age interval', valueField = 'value', labelField = 'label'))
    
    })
  
  
  output$map <- renderLeaflet({
    map <- leaflet(options = leafletOptions(minZoom = 4, maxZoom = 5,zoomControl = FALSE,worldCopyJump = TRUE)) %>% 
      setView(lat = 57.923470, lng = 11.750252, zoom = 4) %>% 
      addTiles(urlTemplate="https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png")
    
  })
  
  colorDomain <- reactive({
    
      timeSpent = data[data$acl00 == input$activitySelect & data$sex == input$genderSelect & data$age == input$ageIntervalSelect,]
      timeSpent = timeSpent %>% group_by(geo) %>% summarize(summ = mean(TIME_SP))
      return(colorNumeric(colorToUse,
                          domain = timeSpent$summ,
                          na.color = "black")
             )
    })
  
  
  
  selectedActivity <<- reactive({
    swapColors()
    return(input$activitySelect)
    })
  
  selectedSex <<- reactive({return(input$genderSelect)})
  
  selectedAgeInterval <<- reactive({return(input$ageIntervalSelect)})
  
  
  
  observe({
    if(input$interactiontype == "1")
    {
      if(selectedActivity() != "" & selectedSex() != "" &  selectedAgeInterval() != "")
      {
        timings = getTimesForChoices(iso3List = polygonData@data$ISO_A3,activity = selectedActivity(),sex = selectedSex(),age = selectedAgeInterval())
        colors <- colorDomain()
        proxy <- leafletProxy("map",session = session)
        proxy %>% clearShapes()
        proxy %>% clearControls()
        proxy %>% addPolygons(data = polygonData,fillColor = ~colors(timings),
                              weight = 2,
                              opacity = 1,
                              color = "#5F678F",
                              label =~labelMaker(polygonData@data$ADMIN,timings),
                              layerId = ~polygonData@data$ISO_A3,
                              fillOpacity = 1.0,highlightOptions = highlightOptions(weight = 5,color = "#313E78",fillOpacity = 0.7,bringToFront = TRUE))
        
        proxy %>% addLegend(data =polygonData ,position="bottomright",
                            pal= colors,
                            values= ~timings,
                            labFormat = function(type, cuts){
                              paste0(periodToPaddedString(lubridate::seconds_to_period(cuts)))
                            } ,
                            opacity = 1.0, title = NULL)
      }
    }
    else
    {
      if(!is.null(input$map_shape_click$id) && input$map_shape_click$.nonce != lastEventSecret)
      {
        lastEventSecret <<- input$map_shape_click$.nonce
        
        proxy <- leafletProxy("map",session = session)
        lat <- input$map_shape_click$lat
        lon <- input$map_shape_click$lng
        
        coords <- as.data.frame(cbind(lon, lat))
        point <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs"))
        
        selectedCountry <- polygonData[point,]
        
        if(input$interactiontype == 2)
        {
          removeShape(proxy,input$map_shape_click$id)
          if(input$map_shape_click$id %in% selectedCountries)
          {
              addPolygons(proxy, data = selectedCountry, 
                                    fillColor = "floralwhite",
                                    weight = 2,
                                    opacity = 1,
                                    color = "#5F678F",
                                    label = selectedCountry@data$ADMIN,
                                    layerId = input$map_shape_click$id,
                                    fillOpacity = 1.0,
                                    highlightOptions = highlightOptions(weight = 5,color = "#313E78",fillOpacity = 0.7,bringToFront = TRUE))
              selectedCountries <<- selectedCountries[selectedCountries != input$map_shape_click$id]
          }
          else
          {
              addPolygons(proxy,data = selectedCountry, 
                                    fillColor = "red",
                                    weight = 2,
                                    opacity = 1,
                                    color = "#5F678F",
                                    label = selectedCountry@data$ADMIN,
                                    layerId = input$map_shape_click$id,
                                    fillOpacity = 1.0,
                                    highlightOptions = highlightOptions(weight = 5,color = "#313E78",fillOpacity = 0.7,bringToFront = TRUE))
              selectedCountries[length(selectedCountries)+1] <<- input$map_shape_click$id
            
          }
        }
        else
        {
          if(input$map_shape_click$id == "Selected")
          {
            removeShape(proxy,"Selected")
            selectedCountries <<- selectedCountries[selectedCountries != input$map_shape_click$id]
          }
          else
          {
            addPolygons(proxy,data = selectedCountry, 
                        fillColor = "red",
                        weight = 2,
                        opacity = 1,
                        color = "#5F678F",
                        label = selectedCountry@data$ADMIN,
                        layerId = "Selected",
                        fillOpacity = 1.0,
                        highlightOptions = highlightOptions(weight = 5,color = "#313E78",fillOpacity = 0.7,bringToFront = TRUE))
            selectedCountries[1] <<- input$map_shape_click$id
          }
          
          if(length(selectedCountries) == 1)
          {
            updateSingleCountryComp()
          }
        }
        
      }
    }
  })

  
  
  updateSingleCountryComp <- function()
  {
    countryIso2 <- getIso2ForIso3(selectedCountries[[1]])
    # make graphs for single country select
    
  }
  
  
  
  
  
  whitePolygons <- function()
  {
    selectedCountries <<- list()
    proxy <- leafletProxy("map",session = session)
    proxy %>% clearShapes()
    proxy %>% clearControls()
    proxy %>% addPolygons(data = polygonData,fillColor = "floralwhite",
                          weight = 2,
                          opacity = 1,
                          color = "#5F678F",
                          label = ~polygonData@data$ADMIN,
                          layerId = ~polygonData@data$ISO_A3,
                          fillOpacity = 1.0,
                          highlightOptions = highlightOptions(weight = 5,color = "#313E78",fillOpacity = 0.7,bringToFront = TRUE))
    
    
  }
  
  observe({
    switch (input$interactiontype,
      "1" = {
        shinyjs::enable("activitySelect")
        shinyjs::enable("genderSelect")
        shinyjs::enable("ageIntervalSelect")
        updateSelectizeInput(inputId = "genderSelect",selected = "T")
      },
      "2" ={
        shinyjs::disable("activitySelect")
        shinyjs::enable("genderSelect")
        shinyjs::enable("ageIntervalSelect")
        whitePolygons()
      },
      "3" = {
        shinyjs::disable("activitySelect")
        shinyjs::disable("genderSelect")
        shinyjs::disable("ageIntervalSelect")
        whitePolygons()
      }
      
    )
  })
}

shinyApp(ui, server)
