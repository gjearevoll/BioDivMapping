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
  
  # Input of species for intensity figure
  observe({
    updateSelectInput(inputId = "species", choices = focalSpeciesDDList[[input$taxa]])
  })
  
  # Species intensity map
  output$speciesMap <- renderPlot({
    simpleName <- redList$species[redList$GBIFName == input$species]
    taxaData <- dataList[[input$taxa]]
    intensityData <- taxaData[[input$intensityType]]
    fillData <- intensityData[[simpleName]]
    
    validate(
      need(!is.null(fillData), paste0("Ingen observasjonsintensitetsdata tilgjengelig for ", simpleName, 
      ". Det er mest sånnsynlig på grunn av en mangel data i relevant datasett."))
    )
    
    fillData2 <- data.frame(intensity = fillData$mean, geometry = fillData$geometry)
    
    colorPalette <- ifelse(input$intensityType == "bias", "viridis", "inferno")
    figureTitle <- ifelse(input$intensityType == "bias", paste0("Observasjonsintensitetkart for *", simpleName, "*"),
                          paste0("Forekomstintensitetskart for *", simpleName, "*"))
    
    intensityPlot <- ggplot(regionGeometry) + 
      geom_sf(fillData2, mapping = aes(colour = intensity, geometry = geometry)) + 
      colorspace::scale_colour_continuous_sequential(palette = colorPalette, na.value = "grey50") +
      geom_sf(fill = NA, lwd = 0.7, colour = "black") +
      theme_bw() +
      labs(colour = "Skalert\nintensitet") + 
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
  
  # Species richness map 
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
  
  # Covvariate map
  output$covariateMap <- renderPlot({

    covariateToPlot <- terra::crop(covariateData[[input$covariate]],
                                   project(vect(regionGeometry), covariateData),
                                   mask = T)
    covariateDataDF <- as.data.frame(project(covariateToPlot, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), xy = TRUE)
    colnames(covariateDataDF)[3] <- "value"
    
    if (input$covariate != "land_cover_corine") {
      scaleFillMiljo <- colorspace::scale_fill_continuous_sequential(palette = "Terrain", na.value = "grey50")
    } else {
      scaleFillMiljo <- scale_fill_discrete(na.value = "grey50", labels = function(x) stringr::str_wrap(x, width = 18))
    }
    ggplot(regionGeometry)+
      geom_raster(data = covariateDataDF[!is.na(covariateDataDF$value),], aes(x = x, y = y, fill = value))  +
      geom_sf(fill = NA, lwd = 1, colour = "black") +
      scaleFillMiljo +
      theme_classic()  +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      guides(fill = guide_legend(ncol = 1))
    
    
  }, height = 500)
  
  # Image box for species intensity tab
  output$imageBox1 <- renderImage({
    simpleName <- redList$species[redList$GBIFName == input$species]
    ImgTxt <- paste0("data/photos/", input$taxa, "/", simpleName,"/speciesImage.jpg")
    if (!file.exists(ImgTxt)) {ImgTxt <- "data/photos/imageNotAvailable.png"}
    list(src = ImgTxt,
         contentType = "image/jpg",
         width = "80%"
    )
  }, deleteFile = FALSE)
  
  # Text box for species intensity tab
  output$textBox1 <- renderUI({
    simpleName <- redList$species[redList$GBIFName == input$species]
    redListStatus <- redList$status[redList$GBIFName == input$species]
    redListStatusFull <- ifelse(redListStatus == "EN", "Sterk truet", ifelse(redListStatus == "VU", "Sårbar", "Kritisk"))
    noOccurrences <- nrow(presenceData[presenceData$acceptedScientificName == input$species,])
    imageUser <- creditList$credit[creditList$species == simpleName]
    imageURL <- creditList$url[creditList$species == simpleName]
    HTML(paste0("<strong>Vitenskapelig navn:</strong> ", simpleName  ,
                "<br/><strong>Tall observasjoner:</strong> ", noOccurrences,
                "<br/><strong>Status på rødlista:</strong> ", redListStatusFull, 
                "<br/><strong>Bildekreditt:</strong> <a href = ", imageURL, ">", imageUser, "<a/>"))
  })
  
  # Caption for species intensity figure
  output$figureCaption1 <- renderText ({
    simpleName <- redList$species[redList$GBIFName == input$species]
    figureCaption <- paste0(ifelse(input$intensityType == "bias", "Observasjons", "Forekomst"), "intensitetkart for ", simpleName, ". Datakilde er Global Biodiversity Information Facility (DOI: ", 
                            downloadKey$doi, ")", ifelse(input$taxa %in% c("vascularPlants", "fungi", "lichens"), " og Arealrepresentativ naturovervåking. ", ". "),
                            "Intensitet er modellert med bruk av Hotspots data arbeidsflyt: https://github.com/gjearevoll/BioDivMapping.",
                            ifelse(input$intensityType == "bias", " Observasjonsbias beregnet bare for forekomstdata.", ""))
    figureCaption
  })
  
  # Display of metadata report
  getPage1<-function() {
    return(includeHTML("speciesMetadata.html"))
  }
  output$inc<-renderUI({getPage1()})
  
  # Display of instructions
  getPage2<-function() {
    return(includeHTML("odbmPipelineDocumentation.html"))
  }
  output$instructions<-renderUI({getPage2()})
 
})



