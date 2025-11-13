library(shiny)
library(leaflet)
library(dplyr)


# Interface utilisateur
fluidPage(
  titlePanel("Locations Airbnb à Seattle"),
  
  sidebarLayout(
    sidebarPanel(
      #Fourchette de prix
      sliderInput(inputId = "prix",
                  label = "Fourchette de prix",
                  min = 15,
                  max = 5900,
                  value = c(15,5900)),
      hr(),
      br(),
      uiOutput("stats")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)