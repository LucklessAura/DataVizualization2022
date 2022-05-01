library(shiny)
library(leaflet)
library(geojsonio)
library(sp)
library(rjson)
library(tidyr)
library(tidyverse)
library(lubridate)
library(shinyjs)
library(ggplot2)

source(file = "./Data.R")
source(file = "./Misc.R")

# polygons for all the countries in the world
fullWorldPolygonData <- geojson_read("./countries.geojson",what = "sp")

# will get filtered to include only polygons of countries in the dataset
polygonData <- fullWorldPolygonData

#cloropleths colors to use
colorToUse <- NULL

# possible periods of time in the dataset
periodChoices <- list("2000","2010","2000 and 2010")

# selected countries in interactions of type 2 and 3 
selectedCountries <- list()

# to be sure that the same click event is not handeled twice 
lastEventSecret <- -1.0


# swaps the possible color interval for the Choropleths, it will always start from floralwhite, no other use than for aesthetics 
swapColors <- function()
{
  colorToUse <<- colorRamp(colorIntervals[[sample(1:length(colorIntervals), 1)]], interpolate="linear")
}


# add to the provided map object either the provided polygons or all the polygons present currently in the polygonData object
# also requires the layerID for the object, fill color for pllygons and labels for polygons
addPolygonsData <- function(map,dataToDisplay = NULL,layerID, polygonFillColor, labels)
{
  if(is.null(dataToDisplay))
  {
    dataToDisplay <- polygonData
  }
  map %>% addPolygons(data = dataToDisplay,fillColor = ~polygonFillColor,
                        weight = 2,
                        opacity = 1,
                        color = "#5F678F",
                        label = ~labels,
                        layerId = ~layerID,
                        fillOpacity = 1.0,
                        highlightOptions = highlightOptions(weight = 5,color = "#313E78",fillOpacity = 0.7,bringToFront = TRUE))
}

# clear current map and add floralwhite polygons corresponding to the data in polygonData
whitePolygons <- function()
{
  selectedCountries <<- list()
  proxy <- leafletProxy("map")
  proxy %>% clearShapes()
  proxy %>% clearControls()
  addPolygonsData(map = proxy,layerID = polygonData@data$ISO_A3,polygonFillColor = "floralwhite",labels = polygonData@data$ADMIN)
}

#defines the layout of the actual web page
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("some title"), #title of the page
  navbarPage("Some Appliction",id = "nav", 
             
  tabPanel("Data Analysis", 
            fluidRow(
              column(12,"Static content goes here"), # static analysis of the data should be put here,for now a fluid row, subject to change if other layout is more appropriate
  )),
  # interactive map to display data
  tabPanel("Interactive",     
    fluidRow(
      column(12,radioButtons("interactionType",label = "Map Use",selected = 1,choices = list("Choropleths" = 1, "Countries Comparison" = 2, "Single Country Select" = 3))),
      column(12,selectizeInput("periodSelect",h3("Select data period"),choices = periodChoices)),
      column(4,selectizeInput("activitySelect",h3("Select the activity"),choices = NULL)),
      column(4,selectizeInput("genderSelect",h3("Select the gender"),choices = NULL)),
      column(4,selectizeInput("ageIntervalSelect",h3("Select the age interval"),choices = NULL)),
      column(12,leafletOutput("map",width = "100%", height = 800)),
      column(12,conditionalPanel(condition = "input.interactionType > 1",plotOutput("hidableGraph")))
    )
  )
)
  
)







# server side 
server <- function(input, output, session) {
  
  #input from user on selects of the interactive page
  observe({
    activity <- selectedActivity()
    sex <- input$genderSelect
    age <- input$ageIntervalSelect
    
    if(activity != "" & sex != "" & age != "")
    {
      if(input$interactionType == "1")
      {
        updateChoropleths(activity,sex,age)
      }
      if(input$interactionType == "2")
      {
        updateHidableGraph()
      }
    }
    
  })
  
  # Choropleths update depending on given data filters 
  updateChoropleths <- function(activity,sex,age)
  {
    # still get called multiple times :(
    
    
    timings = getTimesForChoices(iso3List = polygonData@data$ISO_A3,activity = activity,sex = sex,age = age)
    colors <- colorDomain(activity,sex,age)
    
    proxy <- leafletProxy("map")
    proxy %>% clearShapes()
    proxy %>% clearControls()
    
    addPolygonsData(map = proxy,layerID = polygonData@data$ISO_A3,
                    polygonFillColor = colors(timings),labels = secondsToPeriodLabeler(polygonData@data$ADMIN,timings))
    
    proxy %>% addLegend(data =polygonData ,position="bottomright",
                        pal= colors,
                        values= ~timings,
                        labFormat = function(type, cuts){
                          paste0(periodToPaddedString(lubridate::seconds_to_period(cuts)))
                        } ,
                        opacity = 1.0, title = NULL)
  
    
  }
  
  # basic initialization of the map, depends on the map mode(Choropleths or Basic White Polygons)
  initialize <-function ()
    {
    
    proxy <- leafletProxy("map")
    proxy %>% clearShapes()
    proxy %>% clearControls()
    
    DataReader(input$periodSelect)  
    polygonData <<- fullWorldPolygonData[fullWorldPolygonData@data$ISO_A3 %in% UsedCountriesList,]
    
    updateSelectizeInput(session = session,"activitySelect",choices = UsedActivities,server = TRUE,
                         options = options(closeAfterSelect = TRUE,placeholder = 'Activity', valueField = 'value', labelField = 'label'))
    
    updateSelectizeInput(session = session,"ageIntervalSelect",choices = UsedAgeIntervals,server = TRUE,
                         options = options(closeAfterSelect = TRUE,placeholder = 'Age interval', valueField = 'value', labelField = 'label'))
    
    updateSelectizeInput(session = session,"genderSelect",choices = UsedSexes,server = TRUE,
                         options = options(closeAfterSelect = TRUE,placeholder = 'Sex', valueField = 'value', labelField = 'label'))
    
    
    
    if(input$interactionType != "1")
    {
      whitePolygons();
    }
    
    output$hidableGraph <- NULL
    
  }
  
  # decide which tab of the page to initialize
  observe({
    if(input$nav == "Interactive")
    {
      output$map <- renderLeaflet({
        map <- leaflet(options = leafletOptions(minZoom = 4, maxZoom = 5,zoomControl = FALSE,worldCopyJump = TRUE)) %>% 
          setView(lat = 57.923470, lng = 11.750252, zoom = 4) %>% 
          addTiles(urlTemplate="https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png")
      })
      
      initialize()
    }
    else
    {
      # initialize static content
    }
  })
  
  
  # get the color intervals for the data according to given filters
  colorDomain <- function(act,sex,age)
    {
      timeSpent = data[data$acl00 == act & data$sex == sex & data$age == age,]
      timeSpent = timeSpent %>% group_by(geo) %>% summarize(summ = mean(TIME_SP))
      return(colorNumeric(colorToUse,
                          domain = timeSpent$summ,
                          na.color = "black")
             )
    }
  
  
  # when a different activity is chosen change the color of choropleths, aesthetics cahnge only 
  selectedActivity <<- function(){
    swapColors()
    return(input$activitySelect)
    }
  
  #find which country the click was inside and dispaly data graph for it, also depends on the map interaction mode
  observe({
    if(!is.null(input$map_shape_click$id) && input$map_shape_click$.nonce != lastEventSecret & input$interactionType > 1)
    {
      lastEventSecret <<- input$map_shape_click$.nonce
      
      proxy <- leafletProxy("map",session = session)

      coords <- as.data.frame(cbind(input$map_shape_click$lng, input$map_shape_click$lat))
      point <- SpatialPoints(coords,CRS("+proj=longlat +datum=WGS84 +no_defs"))
      
      selectedCountry <- polygonData[point,]
      
      if(input$interactionType == 2)
      {
        removeShape(proxy,input$map_shape_click$id)
        if(input$map_shape_click$id %in% selectedCountries)
        {
          addPolygonsData(map = proxy,dataToDisplay = selectedCountry,
                          layerID = input$map_shape_click$id,polygonFillColor = "floralwhite",labels = selectedCountry@data$ADMIN)
          selectedCountries <<- selectedCountries[selectedCountries != input$map_shape_click$id]
        }
        else
        {
          addPolygonsData(map = proxy,dataToDisplay = selectedCountry,
                          layerID = input$map_shape_click$id,polygonFillColor = "red",labels = selectedCountry@data$ADMIN)
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
          addPolygonsData(map = proxy,dataToDisplay = selectedCountry,
                          layerID = "Selected",polygonFillColor = "red",labels = selectedCountry@data$ADMIN)
          selectedCountries[1] <<- input$map_shape_click$id
        }
      }
      
      updateHidableGraph()
    }
  },priority = 5)

  #swap interaction type with the map
  observe({
    switch (input$interactionType,
      "1" = {
        shinyjs::enable("activitySelect")
        shinyjs::enable("genderSelect")
        shinyjs::enable("ageIntervalSelect")
        
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
  },priority = 15)
  
  # update the graph for the type 2 and 3 interactions
  updateHidableGraph <- function()
  {
    if(length(selectedCountries) < 1)
    {
      output$hidableGraph <- renderPlot({NULL})
      return()
    }
    
    countriesIso2 <- getIso2ForIso3(selectedCountries)
    if(input$interactionType == "2")
    {
      dataToDisplay = data[data$geo %in% countriesIso2 & data$sex == input$genderSelect & data$age == input$ageIntervalSelect,]
      
      output$hidableGraph <- renderPlot(
        {
          ggplot(data = dataToDisplay,aes(x = geo, y = TIME_SP, fill=geo)) + geom_bar(stat = "identity",position = "dodge") +  
            xlab("Country") + ylab("Time in Seconds")   +  
            facet_wrap(~acl00 ,scales = "free",ncol = 1 ,labeller = labeller(acl00 = toLabelDataframe(UsedActivities))) +
            geom_label(data = dataToDisplay,aes(label = TIME_SP)) +
            theme(strip.text = element_text(size = 15),
                  axis.title.x = element_text(size = 25),
                  axis.title.y = element_text(size = 25),
                  axis.text.y = element_text(size = 15),
                  axis.text.x = element_text(size = 15),
                  panel.spacing.y = unit(1,"cm",data = NULL))
        },height = 27000)
    }
    else
    {
      dataToDisplay = data[data$geo %in% countriesIso2 & data$sex != "T",]
      output$hidableGraph <- renderPlot(
        {
          ggplot(data = dataToDisplay,aes(x = sex, y = TIME_SP,fill=sex)) + 
            geom_bar(stat = "identity",position = "dodge") +  xlab("Sex") + ylab("Time in Seconds")   +  
            facet_grid(acl00 ~ age,scales = "free",labeller=labeller(acl00 = toLabelDataframe(UsedActivities),age = toLabelDataframe(UsedAgeIntervals))) +
            geom_label(data = dataToDisplay,aes(label = TIME_SP)) +
            theme(strip.text = element_text(size = 15),
                  axis.title.x = element_text(size = 25),
                  axis.title.y = element_text(size = 25),
                  axis.text.y = element_text(size = 15),
                  axis.text.x = element_text(size = 15),
                  panel.spacing.y = unit(1,"cm",data = NULL))
        },height = 17000)
    }
    
  }
  
}


# call to build web app
shinyApp(ui, server)
