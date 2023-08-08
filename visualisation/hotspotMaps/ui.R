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
library(raster)

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
                  menuItem("Species Intensities", tabName = "outputs", icon = icon("binoculars")),
                  menuItem("Taxa Biodiversity", tabName = "diversity", icon = icon("worm")),
                  menuItem("Species Occurrences", tabName = "occurrences", icon = icon("bugs")),
                  menuItem("Environmental Covariates", tabName = "covariates", icon = icon("cloud-sun")),
                  menuItem("How it Works", tabName = "howItWorks", icon = icon("circle-question"))
      )
    )
    ,
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "outputs",
                fluidRow(
                  column( width = 3, offset = 0,
                          fluidRow(
                            box(width = 12,title = "Species specs",
                                selectInput(inputId = "taxa", label = "Taxa:",
                                            selected = "fungi",
                                            choices = names(focalSpeciesDDList)),
                                selectInput(inputId = "species", label = "Species:",
                                            selected = "Alectoria_sarmentosa",
                                            choices = focalSpeciesDDList[[1]])))),
                  column(
                    width = 5, offset = 0,
                    box(width = 12,title = "Species Intensity Map",
                        status = "primary",
                        plotOutput("speciesMap"))))
        ),
        tabItem(
          tabName = "diversity",
          fluidRow(
            column( width = 3, offset = 0,
                    fluidRow(
                      box(width = 12,title = "Species specs",
                          selectInput(inputId = "taxa2", label = "Taxa:",
                                      selected = "fungi",
                                      choices = names(focalSpeciesDDList)),
                          checkboxInput(inputId = "selectRedList", 
                                        label = "Use only red-listed species",
                                        value = FALSE)))),
            column(
              width = 5, offset = 0,
              box(width = 12,title = "Taxa Diversity Map",
                  status = "primary",
                  plotOutput("taxaDiversityMap")
              )))
        ),
        tabItem(tabName = "occurrences",
                fluidRow(
                  column(width = 3,
                         fluidRow(
                           box(width = 12,
                               title = "Species",
                               br(),
                               status = "primary",
                               selectInput(inputId = "taxaOccurrence", label = "Taxa:",
                                           selected = names(focalSpeciesDDList)[1],
                                           choices = names(focalSpeciesDDList)),
                               selectInput(inputId = "speciesOccurrence", label = "Species:",
                                           selected = "Alectoria_sarmentosa",
                                           choices = focalSpeciesDDList[[1]]),
                               checkboxInput(inputId = "selectAbsences", 
                                             label = "Show absences",
                                             value = FALSE)   
                           ) 
                         )
                  ),
                  column(width = 9,
                         fluidRow(
                           box(width = 12,
                               title = "Species Occurrence Map",
                               plotOutput("speciesOccurrenceMap")
                           )
                         ))
                )
        )
        ,
        tabItem(tabName = "covariates",
                fluidRow(
                  column(width = 3,
                         fluidRow(
                           box(width = 12,
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
                         )
                  ),
                  column(width = 9,
                         box(
                           title = "Environmental Covariate Map",
                           plotOutput("covariateMap")
                         )
                  )
                )
        ),
        tabItem(tabName = "howItWorks",
                mainPanel(
                  h1("What is the Biodiversity Mapping Tool?"),
                  p("This tool was created as part of an initiative from the Gjærvoll 
                    Centre for Biodiversity Foresight Analyses, as a means of showing 
                    biodiversity hotspots throughout Norway. "),
                  p("More information will be available in this online version of the 
                    tool shortly.")
                  
                )
          
        )
      )
    )
  )
)

