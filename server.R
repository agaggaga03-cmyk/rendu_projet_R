library(shiny)
library(leaflet)
library(leaflet.extras)  # Pour la heatmap
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# CHARGEMENT DES DONNES DU FICHIER CSV ET FILTRAGE
# ------------------------------------------------------------------------------

# Lecture du fichiers csv à l'aide d'une fonction comme ca on peut utiliser tryCatch
charger_donnees <- function(fichier = "seattle.csv") {
  tryCatch({ #On gere l'erreur liée au chargement des donnée grace a TryCatch en renvoyant dans la console le resultat
    airbnb_data <- read.csv("seattle.csv", stringsAsFactors = FALSE) |>
      filter(!is.na(latitude) & !is.na(longitude)) |> #on enleve les valeurs longitude et latitude manquantes
      filter(latitude > 47 & latitude < 48) |> #On garde les coordonnées uniquement vers Seattle
      filter(longitude > -123 & longitude < -122) #Idem
    message(sprintf("✓ Données chargées avec succès : %d locations", nrow(airbnb_data)))
    return(airbnb_data)
  }, error = function(e) {
    stop(sprintf("❌ Erreur lors du chargement des données : %s\nVérifiez que le fichier '%s' existe dans le dossier.", 
                 e$message, fichier))
    })
}

#On charge les donnees au démarrage
airbnb_data <- charger_donnees()
  



function(input, output, session) {
  # Données filtrées en fonction de TOUS les sliders
  airbnb_filtree <- reactive({
    data <- airbnb_data |>
      filter(price >= input$prix[1] & price <= input$prix[2]) |>
      filter(reviews >= input$reviews[1] & reviews <= input$reviews[2]) |>
      filter(accommodates >= input$capacite[1] & accommodates <= input$capacite[2])
    
    # Filtre pour les avis : exclure les NA si on ne sélectionne pas 0
    if (input$avis[1] > 0 | input$avis[2] < 5) {
      data <- data |>
        filter(!is.na(overall_satisfaction)) |>
        filter(overall_satisfaction >= input$avis[1] & overall_satisfaction <= input$avis[2])
    }
    
    return(data)
  })
  
  # Nombre de locations
  output$nb_locations <- renderText({
    paste("Nombre de locations:", nrow(airbnb_filtree()))
  })
  
  output$vb_location <- renderText({
    paste(nrow(airbnb_filtree()))
  })
  
  # Prix moyen
  output$prix_moyen <- renderText({
    paste("Prix moyen: $", round(mean(airbnb_filtree()$price, na.rm = TRUE), 2))
  })
  
  output$vb_prix_moyen <- renderText({
    round(mean(airbnb_filtree()$price, na.rm = TRUE),2)
  })
  
  #Carte de densité géographique des prix
  output$density_map <- renderPlot({
    data_filtered <- airbnb_filtree()
    
    if (nrow(data_filtered) == 0) return(NULL)
    
    ggplot(data_filtered, aes(x = longitude, y = latitude, color = price)) +
      geom_point(alpha = 0.6, size = 2) +
      scale_color_gradient(low = "blue", high = "red", name = "Prix ($)") +
      labs(title = "Répartition géographique des prix",
           x = "Longitude", y = "Latitude") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            legend.background = element_rect(fill = "#34495E"))
  })
  
  #Prix par capacité d'hébergement
  output$prix_capacite <- renderPlot({
    data_filtered <- airbnb_filtree() %>%
      filter(accommodates <= 12)  # Limiter pour clarté
    
    if (nrow(data_filtered) == 0) return(NULL)
    
    prix_par_capacite <- data_filtered %>%
      group_by(accommodates) %>%
      summarise(prix_moyen = mean(price, na.rm = TRUE),
                nb = n())
    
    ggplot(prix_par_capacite, aes(x = factor(accommodates), y = prix_moyen, fill = nb)) +
      geom_col() +
      geom_text(aes(label = paste0("$", round(prix_moyen, 0))), 
                vjust = -0.5, color = "white") +
      scale_fill_gradient(low = "#3498DB", high = "#E74C3C", name = "Nombre") +
      labs(title = "Prix moyen selon la capacité d'accueil",
           x = "Nombre de personnes", y = "Prix moyen ($)") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            legend.background = element_rect(fill = "#34495E"))
  })
  
  #distribution des notes
  output$distribution_notes <- renderPlot({
    data_filtered <- airbnb_filtree() %>%
      filter(!is.na(overall_satisfaction))
    
    if (nrow(data_filtered) == 0) return(NULL)
    
    ggplot(data_filtered, aes(x = overall_satisfaction)) +
      geom_histogram(binwidth = 0.5, fill = "#DF691A", color = "white") +
      labs(title = "Distribution des notes de satisfaction",
           x = "Note (sur 5)", y = "Nombre de locations") +
      scale_x_continuous(breaks = seq(0, 5, 0.5)) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"))
  })
  
  #Nombre d'avis vs prix
  output$reviews_prix <- renderPlot({
    data_filtered <- airbnb_filtree() %>%
      filter(reviews > 0)
    
    if (nrow(data_filtered) == 0) return(NULL)
    
    ggplot(data_filtered, aes(x = reviews, y = price)) +
      geom_point(alpha = 0.4, color = "#3498DB") +
      geom_smooth(method = "loess", color = "#E74C3C", se = TRUE) +
      labs(title = "Relation entre nombre d'avis et prix",
           x = "Nombre d'avis", y = "Prix ($)") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"))
  })
  
  #Top des quartiers les plus chers
  output$top_quartiers <- renderPlot({
    data_filtered <- airbnb_filtree()
    
    # Extraire le quartier depuis l'adresse (simplifié)
    data_filtered$quartier <- sub(",.*", "", data_filtered$address)
    
    top_quartiers <- data_filtered %>%
      group_by(quartier) %>%
      summarise(prix_moyen = mean(price, na.rm = TRUE),
                nb_locations = n()) %>%
      filter(nb_locations >= 10) %>%  # Au moins 10 locations
      arrange(desc(prix_moyen)) %>%
      head(10)
    
    ggplot(top_quartiers, aes(x = reorder(quartier, prix_moyen), y = prix_moyen)) +
      geom_col(fill = "#DF691A") +
      geom_text(aes(label = paste0("$", round(prix_moyen, 0))), 
                hjust = -0.2, color = "white") +
      coord_flip() +
      labs(title = "Top 10 des quartiers les plus chers",
           x = "", y = "Prix moyen ($)") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"))
  })
  
  #Nombre de Logement par quartier
  output$logements_par_quartier <- renderPlot({
    data_filtered <- airbnb_filtree()
    
    if (nrow(data_filtered) == 0) return(NULL)
    
    # Extraire le quartier depuis l'adresse
    data_filtered$quartier <- trimws(sub(",.*", "", data_filtered$address))
    
    # Compter le nombre de logements par quartier
    quartiers_count <- data_filtered %>%
      group_by(quartier) %>%
      summarise(nb_logements = n()) %>%
      arrange(desc(nb_logements)) %>%
      head(15)  # Top 15 quartiers
    
    if (nrow(quartiers_count) == 0) return(NULL)
    
    ggplot(quartiers_count, aes(x = reorder(quartier, nb_logements), y = nb_logements, fill = nb_logements)) +
      geom_col() +
      geom_text(aes(label = nb_logements), 
                hjust = -0.2, color = "white", size = 5) +
      coord_flip() +
      scale_fill_gradient(low = "#3498DB", high = "#E74C3C", name = "Nombre") +
      labs(title = "Nombre de logements par quartier",
           x = "Quartier", y = "Nombre de logements Airbnb") +
      theme_minimal(base_size = 14) +
      theme(plot.background = element_rect(fill = "#2C3E50"),
            panel.background = element_rect(fill = "#34495E"),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            legend.background = element_rect(fill = "#34495E"),
            legend.text = element_text(color = "white"),
            panel.grid = element_line(color = "#404040"))
  })
  
  # Types de logements formatés
  output$vb_types_liste <- renderUI({
    data_filtered <- airbnb_filtree()
    
    if (nrow(data_filtered) == 0) {
      return(tags$div(style = "font-size: 24px;", "Aucun"))
    }
    
    # Compter chaque type
    type_counts <- as.data.frame(table(data_filtered$room_type))
    colnames(type_counts) <- c("Type", "Nombre")
    
    # Extraire les valeurs correctement
    entire <- sum(type_counts$Nombre[type_counts$Type == "Entire home/apt"], na.rm = TRUE)
    private <- sum(type_counts$Nombre[type_counts$Type == "Private room"], na.rm = TRUE)
    shared <- sum(type_counts$Nombre[type_counts$Type == "Shared room"], na.rm = TRUE)
    
    # Si aucune valeur n'est trouvée, mettre 0
    if (length(entire) == 0) entire <- 0
    if (length(private) == 0) private <- 0
    if (length(shared) == 0) shared <- 0
    
     HTML(paste0(
    '<div style="font-size: 12px; line-height: 1.4;">',
    '<div style="margin: 2px 0;"><strong>🏠 Entier :</strong> ', entire, '</div>',
    '<div style="margin: 2px 0;"><strong>🚪 Privé :</strong> ', private, '</div>',
    '<div style="margin: 2px 0;"><strong>👥 Partagé :</strong> ', shared, '</div>',
    '</div>'
  ))
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
    
    # Popups stylisés pour la carte
    popups <- paste0(
      "<div style='font-family: Arial, sans-serif; min-width: 220px; max-width: 300px;'>",
      "<h4 style='margin: 0 0 10px 0; color: #DF691A; font-size: 16px; border-bottom: 2px solid #DF691A; padding-bottom: 5px;'>",
      data_filtered$name, "</h4>",
      
      "<table style='width: 100%; font-size: 13px; line-height: 1.6;'>",
      "<tr>",
      "<td style='padding: 3px 10px 3px 0; color: #7F8C8D; font-weight: bold;'>Type</td>",
      "<td style='padding: 3px 0;'>", data_filtered$room_type, "</td>",
      "</tr>",
      
      "<tr style='background-color: rgba(223, 105, 26, 0.1);'>",
      "<td style='padding: 3px 10px 3px 0; color: #7F8C8D; font-weight: bold;'>Prix</td>",
      "<td style='padding: 3px 0; color: #27AE60; font-weight: bold; font-size: 15px;'>",
      "$", data_filtered$price, " <span style='font-size: 11px; color: #95A5A6;'>/", data_filtered$rate_type, "</span></td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 3px 10px 3px 0; color: #7F8C8D; font-weight: bold;'>Capacité</td>",
      "<td style='padding: 3px 0;'>👥 ", data_filtered$accommodates, " personnes</td>",
      "</tr>",
      
      "<tr style='background-color: rgba(223, 105, 26, 0.1);'>",
      "<td style='padding: 3px 10px 3px 0; color: #7F8C8D; font-weight: bold;'>Chambres</td>",
      "<td style='padding: 3px 0;'>🛏️ ", data_filtered$bedrooms, "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 3px 10px 3px 0; color: #7F8C8D; font-weight: bold;'>Note</td>",
      "<td style='padding: 3px 0;'>", 
      ifelse(!is.na(data_filtered$overall_satisfaction), 
             paste0("⭐ <span style='color: #F39C12; font-weight: bold;'>", data_filtered$overall_satisfaction, "</span>/5"), 
             "❌ <span style='color: #95A5A6;'>Non notée</span>"), 
      "</td>",
      "</tr>",
      
      "<tr style='background-color: rgba(223, 105, 26, 0.1);'>",
      "<td style='padding: 3px 10px 3px 0; color: #7F8C8D; font-weight: bold;'>Avis</td>",
      "<td style='padding: 3px 0;'>💬 ", data_filtered$reviews, "</td>",
      "</tr>",
      
      "</table>",
      "</div>"
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