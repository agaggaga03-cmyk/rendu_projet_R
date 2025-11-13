library(shiny)
library(leaflet)
library(dplyr)

# Lecture du fichiers csv 
airbnb_data <- read.csv("seattle.csv", stringsAsFactors = FALSE) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  filter(latitude > 47 & latitude < 48) %>%
  filter(longitude > -123 & longitude < -122)


function(input, output, session) {
  # on filtre en fonction de la fouchette de prix 
  airbnb_filtree <- reactive({
    airbnb_data |>
      filter(price >= input$prix[1] & price <= input$prix[2])
  })
  
  # Nombre de locations
  output$nb_locations <- renderText({
    paste("Nombre de locations:", nrow(airbnb_filtree()))
  })
  
  # Prix moyen
  output$prix_moyen <- renderText({
    paste("Prix moyen: $", round(mean(airbnb_filtree()$price, na.rm = TRUE), 2))
  })
  
  # Table des types de logements
  output$room_types <- renderTable({
    room_count <- as.data.frame(table(airbnb_filtree()$room_type))
    colnames(room_count) <- c("Type", "Nombre")
    room_count
  })
  
  # statistique
  output$stats <- renderUI({
    req(airbnb_filtree())
    data <- airbnb_filtree()
    
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
    
    data_filtered <- airbnb_filtree()
    
    # Si aucune donnée, afficher une carte vide avec un message
    if (nrow(data_filtered) == 0) {
      return(
        leaflet() %>%
          addTiles() %>%
          setView(lng = -122.335167, lat = 47.608013, zoom = 11) %>%
          addPopups(lng = -122.335167, lat = 47.608013, 
                    popup = "Aucune location trouvée dans cette fourchette de prix")
      )
    }
    
    # Créer les popups avec les informations
    popups <- paste0(
      "<strong>", data_filtered$name, "</strong><br/>",
      "Type: ", data_filtered$room_type, "<br/>",
      "Prix: $", data_filtered$price, " ", data_filtered$rate_type, "<br/>",
      "Capacité: ", data_filtered$accommodates, " personnes<br/>",
      "Chambres: ", data_filtered$bedrooms, "<br/>",
      "Note: ", ifelse(!is.na(data_filtered$overall_satisfaction), 
                       data_filtered$overall_satisfaction, "Non notée"), "<br/>",
      "Avis: ", data_filtered$reviews
    )
    
    # Créer la carte
    leaflet(data = data_filtered) %>%
      addTiles() %>%
      setView(lng = -122.335167, lat = 47.608013, zoom = 11) %>%
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