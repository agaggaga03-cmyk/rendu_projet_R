library(shiny)
library(leaflet)
library(dplyr)
library(bslib)


# Interface utilisateur
fluidPage(
  titlePanel("Locations Airbnb à Seattle"),
  theme = bs_theme(bootswatch = "superhero"),
  
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
    card(
      tabsetPanel(
        tabPanel("Carte de Seattle",
                 br(),
                 h4("Carte de la ville de Seattle"),
                 leafletOutput("map", height = "600px")),
        tabPanel("HeatMap de Seattle",
                 br(),
                 h4("HeatMap de la ville de Seattle"),
                 leafletOutput("heat_map", height = "600px")),
        
        tabPanel("Statistique de Seattle",
               br(),
               h4("Graphique")),
        )
    )
  )
)