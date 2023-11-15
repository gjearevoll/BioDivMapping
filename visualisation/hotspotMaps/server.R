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

# Import all necessary data
dataList <- readRDS("data/outputData.RDS")
#processedDataList <- readRDS("data/processedDataList.RDS")
regionGeometry <- readRDS("data/regionGeometry.RDS")
covariateData <- rast("data/covariateDataList.tiff")
creditList <- readRDS("data/imageCredit.RDS")
speciesRichness <- rast("data/speciesRichnessData.tiff")
redListSpeciesRichness <- rast("data/redListRichnessData.tiff")
presenceData <- readRDS("data/processedPresenceData.RDS")

# # Arrange colours for occurrence points based on data type
# n2 <- length(unique(processedDataCompiled$dataType))
# palette2 <- distinctColorPalette(n2)
# colourFrame2 <- data.frame(dataType = unique(processedDataCompiled$dataType), colour =  palette2)
# colourFrame2$dataTypeFull <- ifelse(colourFrame2$dataType == "PO", "Presence only", "Presence-absence")
# processedDataCompiled$colours2 <- colourFrame2$colour[match(processedDataCompiled$dataType, colourFrame2$dataType)]

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
  # observe({
  #   updateSelectInput(inputId = "speciesOccurrence", choices = focalSpeciesDDList[[input$taxaOccurrence]])
  # })
  
  output$speciesMap <- renderPlot({
    simpleName <- redList$species[redList$GBIFName == input$species]
    taxaData <- dataList[[input$taxa]]
    fillData <- taxaData[[simpleName]]
    
    
    intensityPlot <- ggplot(regionGeometry) + 
      gg(fillData) + 
      geom_sf(fill = NA, lwd = 0.7, colour = "black") +
      colorspace::scale_fill_continuous_sequential(palette = "Inferno", na.value = "grey50") +
      theme_bw() + 
      labs(fill = "Species\nintensity") + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
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
  
  # output$speciesOccurrenceMap <- renderPlot({
  #   
  #   if (input$selectAbsences == TRUE) {
  #     dataToPlot <- processedDataCompiled[processedDataCompiled$simpleScientificName == input$speciesOccurrence,] 
  #   } else {
  #     dataToPlot <- processedDataCompiled[processedDataCompiled$simpleScientificName == input$speciesOccurrence &
  #                                           processedDataCompiled$individualCount == 1,]
  #   }
  #   
  #   basePlot <- ggplot(regionGeometry) +
  #     geom_sf(fill = "white", lwd = 0.7) +
  #     theme_classic() +
  #     theme(legend.text=element_text(size=14),
  #           legend.position = "bottom",
  #           legend.direction = "vertical")
  #   
  #   if (input$dataClassification == "dataSource") {
  #     fullPlot <- basePlot + geom_sf(data = dataToPlot, aes(colour = colours1, labels = name)) +
  #       scale_colour_identity(guide = "legend", breaks = colourFrame1$colour, 
  #                             labels = str_wrap(colourFrame1$name, width = 50))
  #   } else {
  #     fullPlot <- basePlot + geom_sf(data = dataToPlot, aes(colour = colours2, labels = dataType)) +
  #       scale_colour_identity(guide = "legend", breaks = colourFrame2$colour, labels = colourFrame2$dataTypeFull, name = "Data source")
  #   }
  #   
  #   fullPlot
  #   
  # }, height = 600)
  
  output$covariateMap <- renderPlot({

    covariateToPlot <- covariateData[[input$covariate]]
    covariateDataDF <- as.data.frame(covariateToPlot, xy = TRUE)
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
  
  # output$imageBox2 <- renderImage({
  #   if (!file.exists(paste0("data/photos/", input$taxaOccurrence, "/", input$speciesOccurrence,"/speciesImage.jpg"))) {
  #     ImgTxt <- paste0("data/photos/imageNotAvailable.png")
  #   } else {
  #   ImgTxt <- paste0("data/photos/", input$taxaOccurrence, "/", input$speciesOccurrence,"/speciesImage.jpg")
  #   }
  #   list(src = ImgTxt,
  #        contentType = "image/jpg",
  #        width = "80%"
  #   )
  # }, deleteFile = FALSE)
  
  # output$textBox2 <- renderUI({
  #   scientificName <- gsub("_", " ", input$speciesOccurrence)
  #   commonName <- focalSpecies$commonName[focalSpecies$species == input$speciesOccurrence]
  #   redListStatus <- focalSpecies$redListStatus[focalSpecies$species == input$speciesOccurrence]
  #   noOccurrences <- nrow(processedDataCompiled[processedDataCompiled$simpleScientificName == input$speciesOccurrence,])
  #   imageUser <- creditList$credit[creditList$species == input$speciesOccurrence]
  #   imageURL <- creditList$url[creditList$species == input$speciesOccurrence]
  #   HTML(paste0("<strong>Scientific name:</strong> ", scientificName  ,"<br/><strong>Common name:</strong> ", 
  #               commonName, "<br/><strong>Number of occurrences:</strong> ", noOccurrences,
  #               "<br/><strong>Red list status:</strong> ", redListStatus, "<br/><strong>Image Credit:</strong> <a href = ", 
  #               imageURL, ">", imageUser, "<a/>"))
  # })
  
  # output$speciesRichnessMap <- renderPlot({
  #   taxaData <- speciesRichness[[input$taxa2]]
  #   test_df <- as.data.frame(taxaData, xy = TRUE)
  #   colnames(test_df) <- c("x", "y", "value")
  #   
  #   maxScale <- max(test_df$value)
  #   
  #   scaleFill <-  scale_fill_gradient2(low = "white",
  #                                      mid = "blue",
  #                                      high = "red",
  #                                      limits = c(0,maxScale),
  #                                      midpoint = maxScale/2,
  #                                      space = "Lab",
  #                                      na.value = "grey50",
  #                                      guide = "colourbar",
  #                                      aesthetics = "fill")
  #   ggplot(regionGeometry) + 
  #     geom_tile(test_df, mapping = aes(x = x, y = y, fill = value)) + 
  #     geom_sf(fill = NA, lwd = 0.7, colour = "black") +
  #     theme_classic() + 
  #     scaleFill +
  #     labs(fill = "Species\nrichness") + 
  #     theme(axis.title.x = element_blank(),
  #           axis.title.y = element_blank())
  # }, height = 500)
  
  
})



