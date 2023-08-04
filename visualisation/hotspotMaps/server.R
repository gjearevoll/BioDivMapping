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
library(plotKML)

# Import all necessary data
dataList <- readRDS("data/outputData.RDS")
processedDataList <- readRDS("data/processedDataList.RDS")
regionGeometry <- readRDS("data/regionGeometry.RDS")
covariateData <- readRDS("data/covariateDataList.RDS")

# Reorganise occurrence data to read directly into plot
processedDataCompiled <- do.call(rbind, lapply(1:length(processedDataList), FUN = function(x) {
  processedDF <- processedDataList[[x]]
  if (!("individualCount" %in% colnames(processedDF))) {
    processedDF$individualCount <- 1
  }
  subset <- processedDF[,c("simpleScientificName", "individualCount")]
  if ("SHAPE" %in% colnames(subset)) {
    subset <- rename(subset, geometry = SHAPE)
  }
  subset$name <- names(processedDataList)[x]
  subset
}
)) %>% arrange(name)

# Arrange colours for occurrence points
n <- length(unique(processedDataCompiled$name))
palette <- distinctColorPalette(n)
colourFrame <- data.frame(name = unique(processedDataCompiled$name), colour =  palette)
processedDataCompiled$colours <- colourFrame$colour[match(processedDataCompiled$name, colourFrame$name)]

# Create dropdown list for taxa
focalSpecies <- read.csv("data/focalSpecies.csv")
focalSpeciesDDVector <- split(focalSpecies$species, f = focalSpecies$taxonomicGroup)
focalSpeciesDDList <- lapply(focalSpeciesDDVector, FUN = function(z) {
  speciesList <- as.list(z)
  names(speciesList) <- z
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
      
    } else {
      fillData <- dataType
    }
    fillDataTransformed <- reproject(fillData$predictions, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    scaleFill <-  scale_fill_gradient2(low = "darkblue",
                                       mid = "white",
                                       high = "red",
                                       limits = c(0,1),
                                       midpoint = 0.5,
                                       space = "Lab",
                                       na.value = "grey50",
                                       guide = "colourbar",
                                       aesthetics = "fill")
    
      ggplot(regionGeometry) + 
        gg(fillDataTransformed) + 
        geom_sf(fill = NA, lwd = 0.7, colour = "black") +
        scaleFill + 
        theme_classic() + 
        labs(fill = "Species\nintensity") + 
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank())
  })
  
  output$speciesOccurrenceMap <- renderPlot({
    
    if (input$selectAbsences == TRUE) {
      dataToPlot <- processedDataCompiled[processedDataCompiled$simpleScientificName == input$speciesOccurrence,] 
    } else {
    dataToPlot <- processedDataCompiled[processedDataCompiled$simpleScientificName == input$speciesOccurrence &
                                          processedDataCompiled$individualCount == 1,]
    }
    
    ggplot(regionGeometry) +
      geom_sf(fill = "white", lwd = 0.7) +
      geom_sf(data = dataToPlot, aes(colour = colours, labels = name)) +
      theme_classic() +
      theme(legend.text=element_text(size=14)) +
      scale_colour_identity(guide = "legend", breaks = colourFrame$colour, labels = colourFrame$name, name = "Data source")
    
  })
  
  output$covariateMap <- renderPlot({
    
    covariateToPlot <- covariateData[[input$covariate]]
    covariateDataDF <- as.data.frame(covariateToPlot, xy = TRUE) 
    colnames(covariateDataDF)[3] <- "value"
    
    ggplot(regionGeometry)+
      geom_raster(data = covariateDataDF, aes(x = x, y = y, fill = value))  +
      geom_sf(fill = NA, lwd = 1, colour = "black") +
      theme_classic()  +
      theme(axis.title.x=element_blank(), 
            axis.title.y=element_blank()) +
      scale_fill_continuous(na.value = NA)
  })
  
  
})



