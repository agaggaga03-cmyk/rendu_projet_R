library(shiny)
library(leaflet)
library(dplyr)
library(bslib)


# Interface utilisateur
fluidPage(
  titlePanel("Locations Airbnb à Seattle"),
  theme = bs_theme(bootswatch = "superhero"),
  
  #Implementation de la sidebar 
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
      uiOutput("stats"),
      tableOutput("room_types")
    ),
    mainPanel(
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
                 fluidRow(
                   column(6, plotOutput("prix_distribution", height = "400px")),
                   column(6, plotOutput("prix_par_type",height = "400px"))
                 ),
                 fluidRow(
                   column(6, plotOutput("prix_chambres", height = "400px")),
                   column(6, plotOutput("satisfaction_prix",height = "400px"))
                 
                 )
        )
      )
    )
  )
)