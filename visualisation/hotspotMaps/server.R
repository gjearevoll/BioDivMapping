#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(shinyjs)
library(sf)
library(ggplot2)
library(intSDM)

dataList <- readRDS("outputData.RDS")
speciesDataList <- readRDS("speciesDataList.RDS")[["species"]]
regionGeometry <- readRDS("speciesDataList.RDS")[["geometry"]]


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$speciesMap <- renderPlot({
    taxaData <- dataList[[input$taxa]]
    dataType <- taxaData[[input$mapType]]
    
    if (input$mapType == "speciesIntensities") {
      fillData <- dataType[[input$species]]
      scaleFill <-  scale_fill_gradient2(low = "red",
                             mid = "white",
                             high = "blue",
                             limits = c(-2000, 500),
                             space = "Lab",
                             na.value = "grey50",
                             guide = "colourbar",
                             aesthetics = "fill")
    } else {
      fillData <- dataType
      scaleFill <-  scale_fill_gradient2(low = "red",
                                         mid = "white",
                                         high = "blue",
                                         limits = c(-5000, 1000),
                                         space = "Lab",
                                         na.value = "grey50",
                                         guide = "colourbar",
                                         aesthetics = "fill")}
    

    
    ggplot() + gg(fillData$predictions) + scaleFill
  })
  
  output$speciesOccurrenceMap <- renderPlot({
    
    focalSpecies <- input$speciesOccurrence
    projectDirectory <- "/modelOutputs"
    
    workflow <- startWorkflow(
      Projection = '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
      Species = focalSpecies,
      saveOptions = list(projectDirectory = "", projectName =  focalSpecies), Save = FALSE
    )
    workflow$addArea(Object = st_sf(regionGeometry), resolution = '60')
    
    # Add datasets
    for (l in 1:length(speciesDataList)) {
      dataset <- speciesDataList[[l]]
      dataType <- attr(dataset, "dataType")
      datasetName <- paste0("dataset", l, dataType)
      
      workflow$addStructured(dataStructured = dataset,
                             datasetType = dataType,
                             datasetName = datasetName,
                             responseName = 'individualCount',
                             speciesName = 'species')
    }

    
    workflow$plot(Species = TRUE)
  })
  
})
