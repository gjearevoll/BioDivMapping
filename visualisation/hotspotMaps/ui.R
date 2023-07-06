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
                                sliderInput("range", "Range:",
                                            min = -5, max = 5,
                                            value = c(-2,2)),
                                selectInput(inputId = "taxa", label = "Taxa:",
                                            selected = "Vascular plants",
                                            choices = c("Vascular plants" = "vascularPlants")),
                                selectInput(inputId = "species", label = "Species:",
                                            selected = "Agrostis capillaris",
                                            choices = c("Vicia sepium" = "Vicia_sepium",
                                                        "Fraxinus excelsior" = "Fraxinus_excelsior",
                                                        "Ulmus glabra" = "Ulmus_glabra",
                                                        "Juniperus communis" = "Juniperus_communis",
                                                        "Saxifraga aizoides" = "Saxifraga_aizoides",
                                                        "Agrostis capillaris" = "Agrostis_capillaris",
                                                        "Geranium sylvaticum" = "Geranium_sylvaticum",
                                                        "Betula pubescens" = "Betula_pubescens"
                                            ))
                                
                            )
                          )
                  ),
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
                               selectInput(inputId = "speciesOccurrence", label = "Species:",
                                           selected = "Agrostis capillaris",
                                           choices = c("Vicia sepium" = "Vicia_sepium",
                                                       "Fraxinus excelsior" = "Fraxinus_excelsior",
                                                       "Ulmus glabra" = "Ulmus_glabra",
                                                       "Juniperus communis" = "Juniperus_communis",
                                                       "Saxifraga aizoides" = "Saxifraga_aizoides",
                                                       "Agrostis capillaris" = "Agrostis_capillaris",
                                                       "Geranium sylvaticum" = "Geranium_sylvaticum",
                                                       "Betula pubescens" = "Betula_pubescens"
                                           )
                               )
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Species Occurrence Map",
                             plotOutput("speciesOccurrenceMap")
                           )
                         )
                  )
                )
        ),
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
                                                       "Slope" = "slope",
                                                       "Soil moisture" = "soil_moisture",
                                                       "Soil organic carbon" = "soil_organic_carbon",
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