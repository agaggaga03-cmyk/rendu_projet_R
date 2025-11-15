library(shiny)
library(leaflet)
library(dplyr)
library(bslib)


# Interface utilisateur
navbarPage(
  titlePanel("Locations Airbnb à Seattle"),
  theme = bs_theme(bootswatch = "superhero"),
  br(),
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
      sliderInput(inputId = "avis",
                  label = "Note",
                  min = 0,
                  max = 5,
                  value = c(0,5)),
      hr(),
      br(),
      sliderInput(inputId = "reviews",
                  label = "Nombre d'avis",
                  min = 0,
                  max = 687,
                  value = c(0,687)),
      hr(),
      br(),
      sliderInput(inputId = "capacite",
                  label = "Capacite d'hebergement/chambre",
                  min = 1,
                  max = 28,
                  value = c(1,28)),
    ),
    mainPanel(
      fluidRow(
        column(4, 
               value_box(
                 title = "Nombre total de locations",
                 value = textOutput("vb_location"),
                 showcase = bsicons::bs_icon("house-door-fill"),
                 theme = value_box_theme(bg = "#DF6919", fg = "white")
               )
        ),
        column(4, 
               value_box( 
                 title = "Prix moyen :",
                 value = textOutput("vb_prix_moyen"),
                 showcase = bsicons::bs_icon("currency-dollar"),
                 theme = value_box_theme(bg = "#DF6919", fg = "white")
               )
        ),
        column(4, 
               value_box( 
                 title = "Type de Logement :",
                 value = uiOutput("vb_types_liste"),
                 showcase = bsicons::bs_icon("house-door-fill"),
                 theme = value_box_theme(bg = "#DF6919", fg = "white")
               )
        )
      ),
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
                 
                 ),
                 fluidRow(
                   column(6, plotOutput("prix_capacite", height = "400px")),
                   column(6, plotOutput("distribution_notes", height = "400px"))
                 ),
                 fluidRow(
                   column(12, plotOutput("density_map", height = "500px"))
                 ),
                 fluidRow(
                   column(12, plotOutput("top_quartiers", height = "500px"))
                 ),
                 fluidRow(
                   column(12, plotOutput("logements_par_quartier", height = "500px"))
                 )
        )
      )
    )
  )
)