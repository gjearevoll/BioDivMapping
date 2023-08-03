#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

focalSpecies <- read.csv("data/focalSpecies.csv")
focalSpeciesDDVector <- split(focalSpecies$species, f = focalSpecies$taxonomicGroup)
focalSpeciesDDList <- lapply(focalSpeciesDDVector, FUN = function(x) {
  speciesList <- as.list(x)
  names(speciesList) <- x
  speciesList
}
)


# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    
    # HEADER
    dashboardHeader(title = "Norwegian Species Diversity"),
    
    # SIDEBAR
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
                  menuItem("Model Estimates", tabName = "outputs", icon = icon("binoculars")),
                  menuItem("Species Occurrences", tabName = "occurrences", icon = icon("worm")),
                  menuItem("Environmental Covariates", tabName = "covariates", icon = icon("cloud-sun"))
      )
    )
    ,
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "outputs",
                fluidRow(
                  column( width = 3, offset = 0,
                          fluidRow(
                            box(width = 12,title = "Map type",
                                selectInput(inputId = "mapType", label = "Map type:",
                                            selected = "Species diversity",
                                            choices = c("Species intensities" = "speciesIntensities",
                                                        "Species diversity" = "biodiversity"))
                            )
                          ),
                          fluidRow(
                            box(width = 12,title = "Species specs",
                                selectInput(inputId = "taxa", label = "Taxa:",
                                            selected = "fungi",
                                            choices = names(focalSpeciesDDList)),
                                selectInput(inputId = "species", label = "Species:",
                                            selected = "Alectoria_sarmentosa",
                                            choices = focalSpeciesDDList[[1]]))
                            
                          )
                  )
                  ,
                  column(
                    width = 5, offset = 0,
                    box(width = 12,title = "Species Map",
                        status = "primary",
                        plotOutput("speciesMap")
                    )
                  )
                )
                
                
        ),
        tabItem(tabName = "occurrences",
                fluidRow(
                  column(width = 12,
                         fluidRow(
                           box(width = 3,
                               title = "Species",
                               br(),
                               status = "primary",
                               selectInput(inputId = "taxaOccurrence", label = "Taxa:",
                                           selected = names(focalSpeciesDDList)[1],
                                           choices = names(focalSpeciesDDList)),
                               selectInput(inputId = "speciesOccurrence", label = "Species:",
                                           selected = "Alectoria_sarmentosa",
                                           choices = focalSpeciesDDList[[1]])
                           )
                         )
                         ,
                         fluidRow(
                           box(
                             title = "Species Occurrence Map",
                             plotOutput("speciesOccurrenceMap")
                           )
                         )
                  )
                )
        )
        ,
        tabItem(tabName = "covariates",
                fluidRow(
                  column(width = 12,
                         fluidRow(
                           box(width = 3,
                               title = "Environmental Covariates",
                               br(),
                               status = "primary",
                               selectInput(inputId = "covariate", label = "Environmental Covariate:",
                                           selected = "temperature",
                                           choices = c("Aspect" = "aspect",
                                                       "Elevation" = "elevation",
                                                       "Precipitation" = "precipitation",
                                                       "Soil moisture" = "soil_moisture",
                                                       "Temperature" = "temperature"
                                           )
                               )
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Environmental Covariate Map",
                             plotOutput("covariateMap")
                           )
                         )
                  )
                )
        )
      )
    )
  )
)

