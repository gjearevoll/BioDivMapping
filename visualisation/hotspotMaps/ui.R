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
library(intSDM)

dataList <- readRDS("outputData.RDS")

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    
    # HEADER
    dashboardHeader(title = "Norwegian Species Diversity"),
    
    # SIDEBAR
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
                  menuItem("Outputs", tabName = "outputs", icon = icon("dashboard")),
                  menuItem("Inputs", tabName = "widgets", icon = icon("th"))
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
                                            min = -100, max = 100,
                                            value = c(-50,50)),
                                selectInput(inputId = "taxa", label = "Taxa:",
                                            selected = "Vascular plants",
                                            choices = c("Vascular plants" = "vascularPlants")),
                                selectInput(inputId = "species", label = "Species:",
                                            selected = "Arnica montana",
                                            choices = c("Arnica montana" = "Arnica_montana",
                                                        "Fraxinus excelsior" = "Fraxinus_excelsior",
                                                        "Ulmus glabra" = "Ulmus_glabra"))
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
        tabItem(tabName = "widgets",
                h2("Widgets tab content"),
                fluidRow(
                  box(title = "Species Occurrence Map",
                      plotOutput("speciesOccurrenceMap")
                  )
                ),
                fluidRow(
                  
                  box(title = "Species",
                      br(),
                      status = "primary",
                      selectInput(inputId = "speciesOccurrence", label = "Species:",
                                  selected = "Fraxinus_excelsior",
                                  choices = c("Arnica montana" = "Arnica_montana",
                                              "Fraxinus excelsior" = "Fraxinus_excelsior",
                                              "Ulmus glabra" = "Ulmus_glabra"))
                  )
                )
        )
      )
    )
  )
)