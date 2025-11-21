library(shiny)
library(leaflet)
library(bslib)
library(bsicons)


# Interface utilisateur
navbarPage(
  titlePanel("Locations Airbnb à Seattle"),
  theme = bs_theme(bootswatch = "superhero"),
  #CSS pour styliser la navbar
  tags$head(
    tags$style(HTML("
      /* Style de la navbar */
        .navbar {
          background-color: #2C3E50;
            border-bottom: 3px solid #DF691A;
        }
      .navbar-brand {
        color: white !important;
        font-weight: bold !important;
        font-size: 24px !important;
      }
      
      /* Style des sliders */
        .irs--shiny .irs-bar {
          background: #DF691A !important;
            border-color: #DF691A !important;
        }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: #DF691A !important;
      }
      
      /* Style de la sidebar */
        .well {
          background-color: rgba(44, 62, 80, 0.8) !important;
          border: 1px solid #DF691A;
          border-radius: 10px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.3);
        }
      
      /* Amélioration des onglets */
        .nav-tabs > li > a {
          color: white !important;
          background-color: #34495E !important;
        }
      .nav-tabs > li.active > a {
        background-color: #DF691A !important;
          color: white !important;
        font-weight: bold;
      }
      
      /* Style des titres */
        h4 {
          color: #DF691A !important;
            font-weight: bold;
          margin-bottom: 15px;
        }
      
      /* Espacement */
        .tab-content {
          padding-top: 20px;
        }
      "))
  ),
  
  #Creation du panel Dashboard contenant graphes et stats
  tabPanel("Dashboard",
           icon = icon("dashboard"),
           
            #Implementation de la sidebar avec les filtres 
            sidebarLayout(
              sidebarPanel(
                width = 3,
                
                #Tire de la section prix
                div(style = "text-align: center; margin-bottom = 20px;",
                    h3(style = "color: #DF691A; margin: 0;",
                       icon("filter")," Filtres")
                    ),
                #Slider de Fourchette de prix
                
                div(
                  h4("💵 Prix par nuit"),
                  sliderInput(
                    inputId = "prix",
                    label = NULL,
                    min = 15,
                    max = 5900,
                    value = c(15,5900))
                ),
                
                #separateur entre 2 sliders
                
                hr(style = "border-color: #DF691A;"),
                
                #Slider des Note de satisfaction
                
                div(
                  h4("🌟 Note de satisfaction"),
                  sliderInput(inputId = "avis",
                              label = NULL,
                              min = 0,
                              max = 5,
                              value = c(0,5))
                ),
                
                hr(style = "border-color: #DF691A;"),
                
                #Slider du Nombre d'avis
                
                div(
                  h4("📝 Nombre d'avis"),
                  sliderInput(inputId = "reviews",
                              label = NULL,
                              min = 0,
                              max = 687,
                              value = c(0,687))
                ),
                
                hr(style = "border-color: #DF691A;"),
                
                # Slider de la capacité d'accueil
                
                div(
                  h4("👥 Capacité d'accueil"),
                  sliderInput(inputId = "capacite",
                              label = NULL,
                              min = 1,
                              max = 28,
                              value = c(1,28))
                ),
              ),
              
              # MainPanel 
              mainPanel(
                width = 9,
                
                # Les values boxes
                fluidRow(
                  column(4, 
                         value_box(
                           title = "Nombre total de locations",
                           value = textOutput("vb_location"),
                           showcase = bsicons::bs_icon("house-door-fill"),
                           theme = value_box_theme(bg = "#DF6919", fg = "white"),
                           height = "150px"
                         )
                  ),
                  column(4, 
                         value_box( 
                           title = "Prix moyen par nuit",
                           value = textOutput("vb_prix_moyen"),
                           showcase = bsicons::bs_icon("currency-dollar"),
                           theme = value_box_theme(bg = "#3498DB", fg = "white"),
                           height = "150px"
                         )
                  ),
                  column(4, 
                         value_box( 
                           title = "Répartition par type",
                           value = uiOutput("vb_types_liste"),
                           showcase = bsicons::bs_icon("pie-chart-fill"),
                           theme = value_box_theme(bg = "#27AE60", fg = "white"),
                           height = "150px"
                         )
                  )
                ),
                
                br(),
                #Les Onglets principaux ou on peut changer
                tabsetPanel(
                  id = "main_tabs",
                  type = "tabs",
                  
                  #Onglet de la carte de Seattle
                  tabPanel(
                      title = tagList(icon("map-marked-alt"), "Carte"),
                      value = "carte",
                      br(),
                      div(
                        style = "background: #2C3E50; padding: 15px; border-radius: 8px;",
                        h3(style = "color: white; margin: 0;", 
                           icon("map-location-dot"), " Carte de Seattle"),
                        p(style = "color: #BDC3C7; margin: 5px 0 0 0;",
                          "Cliquez sur les points rouges pour voir les détails")
                      ),
                      br(),
                      leafletOutput("map", height = "650px")
                    ),
                  
                  #Onglet de la HeatMap
                  tabPanel(
                    title = tagList(icon("fire"), "Heatmap"),
                    value = "heatmap",
                    br(),
                    div(
                      style = "background: #2C3E50; padding: 15px; border-radius: 8px;",
                      h3(style = "color: white; margin: 0;", 
                         icon("fire"), " Carte de densité"),
                      p(style = "color: #BDC3C7; margin: 5px 0 0 0;",
                        "🔴 Rouge = Prix élevés | 🔵 Bleu = Prix bas")
                    ),
                    br(),     
                    leafletOutput("heat_map", height = "600px")),
                  
                  
                  #Onglet statistiques
                  
                  tabPanel(
                    title = tagList(icon("chart-line"), "Statistiques"),
                    value = "stats",
                    br(),
                    
                    #Section 1: Prix
                      div(
                        style = "background: #2C3E50; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                        h3(style = "color: #DF691A; margin-top: 0;", 
                           icon("dollar-sign"), " Analyse des Prix"),
                        fluidRow(
                          column(6, plotOutput("prix_distribution", height = "400px")),
                          column(6, plotOutput("prix_par_type", height = "400px"))
                        )
                      ),
                    
                    #Section 2: Capacité et chambres
                      
                      div(
                        style = "background: #2C3E50; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                        h3(style = "color: #3498DB; margin-top: 0;", 
                           icon("bed"), " Capacité d'Accueil"),
                        fluidRow(
                          column(6, plotOutput("prix_chambres", height = "400px"))
                        ),
                        fluidRow(
                          column(12, plotOutput("prix_capacite", height = "400px"))
                        )
                      ),
                    
                    # Section 3: Satisfaction
                      div(
                        style = "background: #2C3E50; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                        h3(style = "color: #27AE60; margin-top: 0;", 
                           icon("star"), " Satisfaction Client"),
                        fluidRow(
                          column(6, plotOutput("distribution_notes", height = "400px")),
                          column(6, plotOutput("satisfaction_prix", height = "400px"))
                        )
                      ),
                  
                      # Section 4: Popularité
                      div(
                        style = "background: rgba(52, 73, 94, 0.3); padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                        h3(style = "color: #E74C3C; margin-top: 0;", 
                           icon("comments"), " Popularité"),
                        plotOutput("reviews_prix", height = "450px")
                      ),

                      # Section 6: Quartiers
                    
                      div(
                        style = "background: rgba(52, 73, 94, 0.3); padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                        h3(style = "color: #F39C12; margin-top: 0;", 
                           icon("building"), " Analyse par Quartier"),
                        fluidRow(
                          column(12, plotOutput("top_quartiers", height = "500px"))
                        ),
                        br(),
                        fluidRow(
                          column(12, plotOutput("logements_par_quartier", height = "500px"))
                        )
                      )
                  )
                )
              )
            ) 
                 
  ),
  # Onglet À propos
  tabPanel(
    title = tagList(icon("info-circle"), "À propos"),
    value = "about",
    
    fluidRow(
      column(12,
             div(
               style = "max-width: 900px; margin: 50px auto; padding: 30px; background: rgba(52, 73, 94, 0.5); border-radius: 10px;",
               h2(style = "color: #DF691A;", icon("airbnb"), " À propos de cette application"),
               hr(style = "border-color: #DF691A;"),
               
               h4(style = "color: white;", "📊 Objectif"),
               p(style = "color: #BDC3C7; font-size: 16px;",
                 "Cette application interactive permet d'explorer et d'analyser les données Airbnb de Seattle. 
                 Vous pouvez filtrer les locations selon différents critères(Prix,avis,note,accueil) et visualiser les tendances du marché."),
               
               h4(style = "color: white;", "🎯 Fonctionnalités "),
               tags$ul(style = "color: #BDC3C7; font-size: 16px;",
                       tags$li("Filtres interactifs par prix, note, nombre d'avis et capacité"),
                       tags$li("Cartes interactives avec localisation des Airbnb"),
                       tags$li("Heatmap des prix par zone géographique"),
                       tags$li("Analyses statistiques"),
               ),
               
               h4(style = "color: white;", "🔎 Remarque "),
               p(style = "color: #BDC3C7; font-size: 16px;",
                 "Il y a un trop gros écart entre l'Airbnb le plus cher (5900$) et les autres Airbnb,
                 ce qui graphiquement allonge l'axe des abscisses et ainsi rétrécit certains graphiques.
                 Également la heatmap qui est rouge partout sur Seattle, cela est dû au nombre d'Airbnb
                 placé juste à Seattle, ne pas hésiter à restreindre le scoop et TESTER."),
               
               h4(style = "color: white;", "🚨 Attention"),
               p(style = "color: #BDC3C7; font-size: 16px;",
                 "Éviter de mettre la capacité d'accueil à 1, et ensuite aller dans l'onglet statistiques
                 sinon votre RStudio risquerait de crasher.Nous avons essayer de trouver et resoudre le probleme 
                 mais sans succes.")
             )
      )
    )
  )
  
)
