library(shiny)
library(leaflet)
library(dplyr)

function(input, output, session) {
  
  # Lecture des données
  airbnb_data <- reactive({
    req(input$file)
    
    # Lire le fichier CSV
    data <- read.csv("seattle.csv")
    
    # Nettoyer et filtrer les données
    data <- data %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      filter(latitude > 47 & latitude < 48) %>%
      filter(longitude > -123 & longitude < -122)
    
    return(data)
  })
  
  # Statistiques
  output$stats <- renderUI({
    req(airbnb_data())
    data <- airbnb_data()
    
    tagList(
      h4("Statistiques"),
      p(strong("Nombre de locations:"), nrow(data)),
      p(strong("Prix moyen:"), paste0("$", round(mean(data$price, na.rm = TRUE), 2))),
      p(strong("Types de logements:")),
      tags$ul(
        lapply(table(data$room_type), function(x) {
          tags$li(paste0(names(x), ": ", x))
        })
      )
    )
  })
  
  # Carte
  output$map <- renderLeaflet({
    # Carte par défaut centrée sur Seattle
    leaflet() %>%
      addTiles() %>%
      setView(lng = -122.335167, lat = 47.608013, zoom = 11)
  })
  
  # Observer pour mettre à jour la carte avec les données
  observe({
    data <- airbnb_data()
    req(data)
    
    # Créer les popups avec les informations
    popups <- paste0(
      "<strong>", data$name, "</strong><br/>",
      "Type: ", data$room_type, "<br/>",
      "Prix: $", data$price, " ", data$rate_type, "<br/>",
      "Capacité: ", data$accommodates, " personnes<br/>",
      "Chambres: ", data$bedrooms, "<br/>",
      "Note: ", ifelse(!is.na(data$overall_satisfaction), 
                       data$overall_satisfaction, "Non notée"), "<br/>",
      "Avis: ", data$reviews
    )
    
    # Mettre à jour la carte
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = "red",
        fillColor = "red",
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = popups,
        label = ~name
      )
  })
}