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
library(dplyr)

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
                                         limits = c(input$range),
                                         space = "Lab",
                                         na.value = "grey50",
                                         guide = "colourbar",
                                         aesthetics = "fill")

    } else {
      fillData <- dataType
      scaleFill <-  scale_fill_gradient2(low = "red",
                                         mid = "white",
                                         high = "blue",
                                         limits = c(input$range),
                                         space = "Lab",
                                         na.value = "grey50",
                                         guide = "colourbar",
                                         aesthetics = "fill")}
    
    
    
    ggplot() + gg(fillData$predictions) + scaleFill
  })
  
  output$speciesOccurrenceMap <- renderPlot({
    speciesDataSubset <- lapply(speciesDataList, FUN = function(x) {
      subset <- x[,c("simpleScientificName", "name")]
    }
    )
    speciesDataSubset$ANOData <- rename(speciesDataSubset$ANOData, geometry = SHAPE)
    speciesDataCompiled <- do.call(rbind, speciesDataSubset)
    
    dataToPlot <- speciesDataCompiled[speciesDataCompiled$simpleScientificName == input$speciesOccurrence,]
    
    ggplot(regionGeometry) +
      geom_sf() +
      geom_sf(data = dataToPlot, aes(colour = name))
  })
  
})
