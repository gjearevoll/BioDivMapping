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
library(ggtext)

redList <- readRDS("data/redList.RDS")
redListValid <- redList[redList$valid,]
focalSpeciesDDVector <- split(redListValid$GBIFName, f = redListValid$taxa)
focalSpeciesDDList <- lapply(focalSpeciesDDVector, FUN = function(x) {
  speciesList <- as.list(x)
  names(speciesList) <- x
  speciesList
}
)

source("data/textFunctionsNorsk.R")


# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    
    # HEADER
    dashboardHeader(title = "Hotspots Appen", titleWidth = 400),
    
    # SIDEBAR
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
                  menuItem("Hjemmeside", tabName = "home", icon = icon("house"),
                           menuSubItem("Om verktøy", tabName = "landingPage"),
                           menuSubItem("Arbeidsflyten", tabName = "instructions")#,
                          # menuSubItem("FAQs", tabName = "faqs"),
                          # menuSubItem("Contact", tabName = "contactPage")
                          ),
                  menuItem("Artsintensiteter", tabName = "intensity", icon = icon("binoculars")),
                  menuItem("Observert artsrikhet", tabName = "diversity", icon = icon("worm")),
                  menuItem("Miljøvariabler", tabName = "covariates", icon = icon("cloud-sun")),
                  menuItem("Metadata", tabName = "meta", icon = icon("file"))
      )
    )
    ,
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "landingPage",
                fluidRow(box(landingPageText1(), width = 12)),
                fluidRow(box(landingPageText2(), title = "Mer informasjon", width = 12, 
                             collapsible = TRUE, collapsed = TRUE))),
        tabItem(tabName = "instructions",
                fluidRow(
                  column(12,
                         htmlOutput("instructions")
                  ))),
        
        tabItem(tabName = "intensity",
                fluidRow(
                  column( width = 3, offset = 0,
                          fluidRow(
                            box(width = 12,title = "Velg art",
                                selectInput(inputId = "taxa", label = "Taxa:",
                                            selected = "spiders",
                                            choices = names(focalSpeciesDDList)),
                                selectInput(inputId = "species", label = "Art:",
                                            selected = "Arctosa cinerea (Fabricius, 1777)",
                                            choices = focalSpeciesDDList[[4]]),
                                checkboxInput(inputId = "showOccurrences",
                                              label = "Vis forekomster",
                                              value = FALSE))),
                          fluidRow(
                            box(width = 12, title = "Velg intensitetstype",
                                selectInput(inputId = "intensityType", label = "Intensitetstypee:",
                                            selected = "speciesIntensities",
                                            choices = c("Forekomstintensitet" = "speciesIntensities",
                                                        "Observasjonsintensitet" = "bias")))
                          ),
                          fluidRow(
                            box(width = 12, title = "Artsinformasjon",
                                collapsible = TRUE,
                                htmlOutput("textBox1"),
                                p(),
                                imageOutput("imageBox1"))
                          )
                  ),
                  column(
                    width = 5, offset = 0,
                    fluidRow(
                      box(width = 12,title = "Artsintensitskart",
                          status = "primary",
                          plotOutput("speciesMap", height = '100%'),
                      p(),
                      textOutput("figureCaption1"))),
                    fluidRow(
                      box(width = 12, title = "Hva er artsintensitet?",
                          collapsible = TRUE, collapsed = TRUE,
                          speciesIntensityText()))))
        ),
        tabItem(
          tabName = "diversity",
          fluidRow(
            column( width = 3, offset = 0,
                    fluidRow(
                      box(width = 12,title = "Velg taxa",
                          selectInput(inputId = "taxa2", label = "Taxa:",
                                      selected = "spiders",
                                      choices = names(focalSpeciesDDList)),
                          checkboxInput(inputId = "selectRedList",
                                        label = "Bruk bare trua arter",
                                        value = FALSE)))),
            column(
              width = 5, offset = 0,
              box(width = 12,title = "Artsrikhetkart",
                  status = "primary",
                  plotOutput("speciesRichnessMap", height = '100%')
              ))
            )
        )
        ,
        tabItem(tabName = "covariates",
                fluidRow(
                  column(width = 3,
                         fluidRow(
                           box(width = 12,
                               title = "Miljøvariabler",
                               status = "primary",
                               selectInput(inputId = "covariate", label = "",
                                           selected = "temperature",
                                           choices = c("Aspekt" = "aspect",
                                                       "Høyde" = "elevation",
                                                       "Nedbor" = "precipitation",
                                                       "Jordfuktighet" = "soil_moisture",
                                                       "Temperatur" = "temperature",
                                                       "Skogskarboninnhold" = "forest_carbon",
                                                       "Jordgrovhet" = "soil_coarse_fraction",
                                                       "Skråning" = "slope",
                                                       "Netto primærproduktivitet" = "net_primary_productivity",
                                                       "Landbruk" = "land_cover_corine"
                                           )
                               )
                           )
                         )
                  ),
                  column(width = 9,
                         fluidRow(
                           box(
                             title = "Miljøvariablerkart",
                             plotOutput("covariateMap", height = "100%")
                           )
                         ),
                         fluidRow(
                           box(
                             title = "Hvor kommer miljødata fra?",
                             environmentalCovariateText(), collapsible = TRUE,
                             collapsed = TRUE)
                         )

                  )
                )
        ),
        tabItem(tabName = "meta",
                fluidRow(
                  column(12,
                         htmlOutput("inc")
                  ))
        )
        
      )
    )
  )
)


