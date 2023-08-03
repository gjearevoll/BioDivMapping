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
library(dplyr)
library(inlabru)
library(randomcoloR)

dataList <- readRDS("data/outputData.RDS")
speciesDataList <- readRDS("data/speciesDataList.RDS")[["species"]]
regionGeometry <- readRDS("data/regionGeometry.RDS")
covariateData <- readRDS("data/covariateDataList.RDS")

speciesDataSubset <- lapply(speciesDataList, FUN = function(x) {
  subset <- x[,c("simpleScientificName", "name")]
}
)
speciesDataSubset$ANOData <- rename(speciesDataSubset$ANOData, geometry = SHAPE)
speciesDataCompiled <- do.call(rbind, speciesDataSubset) %>% arrange(name)

n <- length(unique(speciesDataCompiled$name))
palette <- distinctColorPalette(n)
colourFrame <- data.frame(name = unique(speciesDataCompiled$name), colour =  palette)

speciesDataCompiled$colours <- colourFrame$colour[match(speciesDataCompiled$name, colourFrame$name)]

focalSpecies <- read.csv("data/focalSpecies.csv")
focalSpeciesDDVector <- split(focalSpecies$species, f = focalSpecies$taxonomicGroup)
focalSpeciesDDList <- lapply(focalSpeciesDDVector, FUN = function(x) {
  speciesList <- as.list(x)
  names(speciesList) <- x
  speciesList
}
)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  observe({
    updateSelectInput(inputId = "species", choices = focalSpeciesDDList[[input$taxa]])
  })
  observe({
    updateSelectInput(inputId = "speciesOccurrence", choices = focalSpeciesDDList[[input$taxaOccurrence]])
  })
  
  output$speciesMap <- renderPlot({
    taxaData <- dataList[[input$taxa]]
    dataType <- taxaData[[input$mapType]]
    
    if (input$mapType == "speciesIntensities") {
      fillData <- dataType[[input$species]]
      scaleFill <-  scale_fill_gradient2(low = "red",
                                         mid = "white",
                                         high = "blue",
                                         limits = c(0,1),
                                         midpoint = 0.5,
                                         space = "Lab",
                                         na.value = "grey50",
                                         guide = "colourbar",
                                         aesthetics = "fill")
      
    } else {
      fillData <- dataType
      scaleFill <-  scale_fill_gradient2(low = "red",
                                         mid = "white",
                                         high = "blue",
                                         limits = c(0,1),
                                         midpoint = 0.5,
                                         space = "Lab",
                                         na.value = "grey50",
                                         guide = "colourbar",
                                         aesthetics = "fill")}
    
    
    
    ggplot() + gg(fillData$predictions) + scaleFill
  })
  
  output$speciesOccurrenceMap <- renderPlot({
    
    dataToPlot <- speciesDataCompiled[speciesDataCompiled$simpleScientificName == input$speciesOccurrence,]
    datasetsInData <- unique(dataToPlot$name)
    
    ggplot(regionGeometry) +
      geom_sf() +
      geom_sf(data = dataToPlot, aes(colour = colours, labels = name)) +
      scale_colour_identity(guide = "legend", breaks = colourFrame$colour, labels = colourFrame$name)
  })
  
  output$covariateMap <- renderPlot({
    
    covariateToPlot <- covariateData[[input$covariate]]
    covariateDataDF <- as.data.frame(covariateToPlot, xy = TRUE) 
    colnames(covariateDataDF)[3] <- "value"
    
    ggplot(regionGeometry)+
      geom_raster(data = covariateDataDF, aes(x = x, y = y, fill = value))  +
      geom_sf(fill = NA, lwd = 1, colour = "black") +
      theme_light()  +
      theme(axis.title.x=element_blank(), 
            axis.title.y=element_blank()) +
      scale_fill_continuous(na.value = NA)
  })
  
  
})



