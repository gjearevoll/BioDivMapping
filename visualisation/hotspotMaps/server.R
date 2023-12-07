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
library(raster)
library(terra)
library(tidyterra)
library(ggtext)

# Import all necessary data
dataList <- readRDS("data/outputData.RDS")
regionGeometry <- readRDS("data/regionGeometry.RDS")
covariateData <- rast("data/covariateDataList.tiff")
creditList <- readRDS("data/imageCredit.RDS")
speciesRichness <- rast("data/speciesRichnessData.tiff")
redListSpeciesRichness <- rast("data/redListRichnessData.tiff")
presenceData <- readRDS("data/processedPresenceData.RDS")
downloadKey <- readRDS("data/downloadKey.RDS")


# Create dropdown list for taxa
redList <- readRDS("data/redList.RDS")
redListValid <- redList[redList$valid,]
focalSpeciesDDVector <- split(redListValid$GBIFName, f = redListValid$taxa)
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
  
  output$speciesMap <- renderPlot({
    simpleName <- redList$species[redList$GBIFName == input$species]
    taxaData <- dataList[[input$taxa]]
    intensityData <- taxaData[[input$intensityType]]
    fillData <- intensityData[[simpleName]]
    
    validate(
      need(!is.null(fillData), paste0("No sampling intensity data available for ", simpleName, 
      ". The most likely cause is a lack of data in participatory datasets."))
    )
    
    fillData2 <- data.frame(intensity = fillData$mean, geometry = fillData$geometry)
    
    colorPalette <- ifelse(input$intensityType == "bias", "viridis", "inferno")
    figureTitle <- ifelse(input$intensityType == "bias", paste0("Sampling intensity for *", simpleName, "*"),
                          paste0("Occurrence intensity map for *", simpleName, "*"))
    
    intensityPlot <- ggplot(regionGeometry) + 
      geom_sf(fillData2, mapping = aes(colour = intensity, geometry = geometry)) + 
      colorspace::scale_colour_continuous_sequential(palette = colorPalette, na.value = "grey50") +
      geom_sf(fill = NA, lwd = 0.7, colour = "black") +
      theme_bw() +
      labs(colour = "Scaled\nintensity") + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = ggtext::element_markdown()) +
      labs(title = figureTitle)
    
    if (input$showOccurrences == TRUE) {
      dataToPlot <- presenceData[presenceData$acceptedScientificName == input$species,] 
      intensityPlot <- intensityPlot + geom_sf(data = dataToPlot, colour = "green")
    }
    intensityPlot
    
  }, height = 500)
  
  output$speciesRichnessMap <- renderPlot({

    if (input$selectRedList == TRUE) {
      fillData <- redListSpeciesRichness[[input$taxa2]]
    } else {
      fillData <- speciesRichness[[input$taxa2]]
    }
    
    speciesRichnessAggregated <- aggregate(fillData, fact = 3)
    richnessPlot <- ggplot(regionGeometry) +
      geom_spatraster(data = speciesRichnessAggregated) +
      colorspace::scale_fill_continuous_sequential(palette = "Viridis", na.value=NA) +
      geom_sf(fill = "NA") +
      theme_bw()
    
    richnessPlot

  }, height = 500)
  
  output$covariateMap <- renderPlot({

    covariateToPlot <- terra::crop(covariateData[[input$covariate]],
                                   project(vect(regionGeometry), covariateData),
                                   mask = T)
    covariateDataDF <- as.data.frame(project(covariateToPlot, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), xy = TRUE)
    colnames(covariateDataDF)[3] <- "value"

    ggplot(regionGeometry)+
      geom_raster(data = covariateDataDF[!is.na(covariateDataDF$value),], aes(x = x, y = y, fill = value))  +
      geom_sf(fill = NA, lwd = 1, colour = "black") +
      colorspace::scale_fill_continuous_sequential(palette = "Terrain", na.value = "grey50") +
      theme_classic()  +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())
  }, height = 500)
  
  output$imageBox1 <- renderImage({
    simpleName <- redList$species[redList$GBIFName == input$species]
    ImgTxt <- paste0("data/photos/", input$taxa, "/", simpleName,"/speciesImage.jpg")
    if (!file.exists(ImgTxt)) {ImgTxt <- "data/photos/imageNotAvailable.png"}
    list(src = ImgTxt,
         contentType = "image/jpg",
         width = "80%"
    )
  }, deleteFile = FALSE)
  
  output$textBox1 <- renderUI({
    simpleName <- redList$species[redList$GBIFName == input$species]
    redListStatus <- redList$status[redList$GBIFName == input$species]
    redListStatusFull <- ifelse(redListStatus == "EN", "Endangered", ifelse(redListStatus == "VU", "Vulnerable", "Critical"))
    noOccurrences <- nrow(presenceData[presenceData$acceptedScientificName == input$species,])
    imageUser <- creditList$credit[creditList$species == simpleName]
    imageURL <- creditList$url[creditList$species == simpleName]
    HTML(paste0("<strong>Scientific name:</strong> ", simpleName  ,
                "<br/><strong>Number of occurrences:</strong> ", noOccurrences,
                "<br/><strong>Red list status:</strong> ", redListStatusFull, 
                "<br/><strong>Image Credit:</strong> <a href = ", imageURL, ">", imageUser, "<a/>"))
  })
  
  output$figureCaption1 <- renderText ({
    simpleName <- redList$species[redList$GBIFName == input$species]
    figureCaption <- paste0(ifelse(input$intensityType == "bias", "Sampling ", "Occurrence "), "intensity map for ", simpleName, ". Data is drawn from the Global Biodiversity Information Facility (DOI: ", 
                            downloadKey$doi, ")", ifelse(input$taxa %in% c("vascularPlants", "fungi", "lichens"), " and Arealrepresentativ naturovervåking. ", ". "),
                            "Species modelled produced using the Hotspots data pipeline: https://github.com/gjearevoll/BioDivMapping.",
                            ifelse(input$intensityType == "bias", " Sampling bias calculated for participatory datasets only.", ""))
    figureCaption
  })
  
  getPage<-function() {
    return(includeHTML("speciesMetadata.html"))
  }
  output$inc<-renderUI({getPage()})
 
})



