library(shiny)
library(leaflet)
library(dplyr)


# Interface utilisateur
fluidPage(
  titlePanel("Locations Airbnb à Seattle"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Charger le fichier CSV",
                accept = c(".csv")),
      hr(),
      helpText("Chargez votre fichier CSV contenant les données Airbnb."),
      br(),
      uiOutput("stats")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)