library(shiny)
library(leaflet)
library(leaflet.extras)  # Pour la heatmap
library(dplyr)
library(ggplot2)

# Lecture du fichiers csv 
airbnb_data <- read.csv("seattle.csv", stringsAsFactors = FALSE) |>
  filter(!is.na(latitude) & !is.na(longitude)) |> #on gere les valeurs manquantes
  filter(latitude > 47 & latitude < 48) |> #Pour se restreindre uniquement a Seattle
  filter(longitude > -123 & longitude < -122) #Pareil


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
  output$room_types <- renderUI({
    data_filtered <- airbnb_filtree()
    
    if (nrow(data_filtered) == 0) {
      return(p("Aucune location dans cette fourchette"))
    }
    
    room_count <- as.data.frame(table(data_filtered$room_type))
    colnames(room_count) <- c("Type", "Nombre")
    
    tagList(
      tags$table(class = "table table-striped",
                 tags$thead(
                   tags$tr(
                     tags$th("Type"),
                     tags$th("Nombre")
                   )
                 ),
                 tags$tbody(
                   lapply(1:nrow(room_count), function(i) {
                     tags$tr(
                       tags$td(room_count[i, 1]),
                       tags$td(room_count[i, 2])
                     )
                   })
                 )
      )
    )
  })
  
  # statistique
  output$stats <- renderUI({
    req(airbnb_filtree())
    data <- airbnb_filtree()
    
    tagList(
      h4("Statistiques"),
      p(strong("Nombre de locations:"), nrow(data)),
      p(strong("Prix moyen:"), paste0("$", round(mean(data$price, na.rm = TRUE), 2)))
    )
  })
  
  # Distribution des prix
  output$prix_distribution <- renderPlot({
    data_filtered <- airbnb_filtree()
    
    ggplot(data_filtered, aes(x = price)) +
      geom_histogram(binwidth = 20, fill = "#E74C3C", color = "white") +
      labs(title = "Distribution des prix", x = "Prix ($)", y = "Fréquence") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"))
  })
  
  # Prix moyen par type
  output$prix_par_type <- renderPlot({
    data_filtered <- airbnb_filtree()
    
    prix_par_type <- data_filtered %>%
      group_by(room_type) %>%
      summarise(prix_moyen = mean(price, na.rm = TRUE))
    
    ggplot(prix_par_type, aes(x = reorder(room_type, prix_moyen), y = prix_moyen, fill = room_type)) +
      geom_col() +
      coord_flip() +
      labs(title = "Prix moyen par type", x = "", y = "Prix moyen ($)") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"))
  })
  
  # Prix par nombre de chambres
  output$prix_chambres <- renderPlot({
    data_filtered <- airbnb_filtree() %>%
      filter(!is.na(bedrooms) & bedrooms <= 6)
    
    ggplot(data_filtered, aes(x = factor(bedrooms), y = price, fill = factor(bedrooms))) +
      geom_boxplot() +
      labs(title = "Prix selon nombre de chambres", x = "Chambres", y = "Prix ($)") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"))
  })
  
  # Satisfaction vs Prix
  output$satisfaction_prix <- renderPlot({
    data_filtered <- airbnb_filtree() %>%
      filter(!is.na(overall_satisfaction))
    
    ggplot(data_filtered, aes(x = overall_satisfaction, y = price)) +
      geom_point(alpha = 0.4, color = "#3498DB") +
      geom_smooth(method = "lm", color = "#E74C3C") +
      labs(title = "Satisfaction vs Prix", x = "Note", y = "Prix ($)") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"))
  })
  
  
  
  
  # Carte Simple
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
    leaflet(data = data_filtered) |>
      addTiles() |>
      setView(lng = -122.335167, lat = 47.608013, zoom = 11) |>
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
  
  #Carte HeatMap
  
  # Heatmap en fonction du prix
  output$heat_map <- renderLeaflet({
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
    
    # Créer la heatmap avec les prix comme intensité
    leaflet(data = data_filtered) %>%
      addTiles() %>%
      setView(lng = -122.335167, lat = 47.608013, zoom = 11) %>%
      addHeatmap(
        lng = ~longitude,
        lat = ~latitude,
        intensity = ~price,
        blur = 20,
        max = 0.05,
        radius = 15
      )
  })
}