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
                fluidRow(box(landingPageText1(), width = 12)),
                fluidRow(box(landingPageText2(), title = "How does it work?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(landingPageText3(), title = "What's an Integrated Species Distribution Model?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(landingPageText4(), title = "Who is the tool designed for?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(landingPageText5(), title = "Future development", width = 12, 
                             collapsible = TRUE, collapsed = TRUE))),
        tabItem(tabName = "instructions",
                fluidRow(box(instructionsText1(), width = 12)),
                fluidRow(box(instructionsText2(), title = "Setting up your data", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(instructionsText3(), title = "Running the pipeline", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(instructionsText4(), title = "Creating your own app", width = 12, 
                             collapsible = TRUE, collapsed = TRUE))),
        tabItem(tabName = "faqs",
                fluidRow(box(faqText1(), width = 12)),
                fluidRow(box(speciesIntensityText(), title = "What is species intensity?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(faqText2(), title = "What is an integrated distribution model?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(faqText3(), title = "How do you integrate presence-only data with presence-absence data?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(faqText4(), title = "How do I find the GBIF taxa number?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(faqText5(), title = "How do I find the GBIF dataset code?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(faqText6(), title = "How do I get the pipeline onto my computer?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(faqText7(), title = "What's an accepted scientific name?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(faqText8(), title = "What is a (good) INLA mesh?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE)),
                fluidRow(box(faqText9(), title = "What parameters should I give when building an INLA mesh?", width = 12, 
                             collapsible = TRUE, collapsed = TRUE))),
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
                          plotOutput("speciesMap", height = '100%'))),
                    fluidRow(
                      box(width = 12, title = "What is species intensity?",
                          collapsible = TRUE, collapsed = TRUE,
                          speciesIntensityText()))))
        ),
        tabItem(
          tabName = "diversity",
          fluidRow(
            column( width = 2, offset = 0,
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
                  plotOutput("taxaDiversityMap", height = '100%')
              )),
            column(
              width = 5, offset = 0,
              box(width = 12,title = "Taxa Richness Map",
                  plotOutput("speciesRichnessMap", height = '100%')
              ))
            )
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
                           box(
                             title = "Species Occurrence Map",
                             plotOutput("speciesOccurrenceMap", height = "100%")
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
                         fluidRow(
                           box(
                             title = "Environmental Covariate Map",
                             plotOutput("covariateMap", height = "100%")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Where do these covariates come from?",
                             environmentalCovariateText(), collapsible = TRUE,
                             collapsed = TRUE)
                         )
                         
                  )
                )
        )
        
      )
    )
  )
)


