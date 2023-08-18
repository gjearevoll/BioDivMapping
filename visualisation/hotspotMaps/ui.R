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

source("data/textFunctions.R")


# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    
    # HEADER
    dashboardHeader(title = "The Open Data Biodiversity Mapper", titleWidth = 400),
    
    # SIDEBAR
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
                  menuItem("Home", tabName = "home", icon = icon("house"),
                           menuSubItem("About the tool", tabName = "landingPage"),
                           menuSubItem("Instructions", tabName = "instructions"),
                           menuSubItem("FAQs", tabName = "faqs"),
                           menuSubItem("Contact", tabName = "contactPage")),
                  menuItem("Species Intensities", tabName = "intensity", icon = icon("binoculars")),
                  menuItem("Taxa Biodiversity", tabName = "diversity", icon = icon("worm")),
                  menuItem("Species Occurrences", tabName = "occurrences", icon = icon("bugs")),
                  menuItem("Environmental Covariates", tabName = "covariates", icon = icon("cloud-sun"))
      )
    )
    ,
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "landingPage",
                box(landingPageText1(), width = 12),
                box(landingPageText2(), title = "How does it work?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(landingPageText3(), title = "What's an Integrated Species Distribution Model?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(landingPageText4(), title = "Who is the tool designed for?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE)),
        tabItem(tabName = "instructions",
                box(instructionsText1(), width = 12),
                box(instructionsText2(), title = "Setting up your data", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(instructionsText3(), title = "Running the pipeline", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(instructionsText4(), title = "Creating your own app", width = 12, 
                    collapsible = TRUE, collapsed = TRUE)),
        tabItem(tabName = "faqs",
                box(faqText1(), width = 12),
                box(speciesIntensityText(), title = "What is species intensity?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(faqText2(), title = "What is an integrated distribution model?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(faqText3(), title = "How do you integrate presence-only data with presence-absence data?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(faqText4(), title = "How do I find the GBIF taxa number?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(faqText5(), title = "How do I find the GBIF dataset code?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(faqText6(), title = "How do I get the pipeline onto my computer?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(faqText7(), title = "What's an accepted scientific name?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(faqText8(), title = "What is a (good) INLA mesh?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE),
                box(faqText9(), title = "What parameters should I give when building an INLA mesh?", width = 12, 
                    collapsible = TRUE, collapsed = TRUE)),
        tabItem(tabName = "contactPage",
                box(contactText(), width = 12)),
        
        tabItem(tabName = "intensity",
                fluidRow(
                  column( width = 3, offset = 0,
                          fluidRow(
                            box(width = 12,title = "Select species",
                                selectInput(inputId = "taxa", label = "Taxa:",
                                            selected = "fungi",
                                            choices = names(focalSpeciesDDList)),
                                selectInput(inputId = "species", label = "Species:",
                                            selected = "Alectoria_sarmentosa",
                                            choices = focalSpeciesDDList[[1]]),
                                checkboxInput(inputId = "showOccurrences",
                                              label = "Show occurrences",
                                              value = FALSE))),
                          fluidRow(
                            box(width = 12, title = "Species Info",
                                collapsible = TRUE,
                                htmlOutput("textBox1"),
                                p(),
                                imageOutput("imageBox1"))
                          )
                  ),
                  column(
                    width = 5, offset = 0,
                    fluidRow(
                      box(width = 12,title = "Species Intensity Map",
                          status = "primary",
                          plotOutput("speciesMap"))),
                    fluidRow(
                      box(width = 12, title = "What is species intensity?",
                          collapsible = TRUE, collapsed = TRUE,
                          speciesIntensityText()))))
        ),
        tabItem(
          tabName = "diversity",
          fluidRow(
            column( width = 3, offset = 0,
                    fluidRow(
                      box(width = 12,title = "Select taxa",
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
                               title = "Select species",
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
                         ),
                         fluidRow(
                           box(width = 12,
                               title = "Data type",
                               selectInput(inputId = "dataClassification", label = "Show observations by:",
                                           selected = "Data source",
                                           choices = c("Data source" = "dataSource",
                                                       "Data type" = "dataType"))
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Species Info",
                               collapsible = TRUE,
                               htmlOutput("textBox2"),
                               p(),
                               imageOutput("imageBox2"))
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
                               selectInput(inputId = "covariate", label = "",
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
        )
        
      )
    )
  )
)


