# ==============================================================================
# Funktion: Karte der Residuen erstellen (Geovisualisierung)
# ==============================================================================
# Visualisiert die räumliche Verteilung der Modellfehler.
# Flexibilität: Funktioniert für West, Ost oder Gesamtdeutschland.
# Voraussetzung: 'data' muss ein sf-Objekt (Shapefile) sein!
# ==============================================================================

plot_residual_map <- function(model, region, modelnummer) {
  
  # ---------------------------------------------------------
  # A. Daten filtern (Flexibel für West/Ost/Gesamt)
  # ---------------------------------------------------------
  
  # Wir prüfen, ob "gesamt" oder "deutschland" im Regions-Namen steckt.
  # Falls ja: Kein Filter (wir nehmen alle Wahlkreise).
  is_gesamt <- grepl("gesamt", tolower(region))
  
  if (is_gesamt) {
    message(">>> Erstelle Karte für Gesamtdeutschland...")
    df_map <- data # Keine Filterung
  } else {
    # Falls nicht gesamt, prüfen wir ob West oder Ost
    target_ost <- if (grepl("west", tolower(region))) 0 else 1
    
    df_map <- data %>%
      dplyr::filter(ost == target_ost)
  }
  
  # ---------------------------------------------------------
  # B. Residuen berechnen
  # ---------------------------------------------------------
  # WICHTIG: Das Modell muss zur Region passen!
  
  df_map <- df_map %>%
    dplyr::mutate(
      pred_temp = predict(model, newdata = ., type = "response"),
      # Umrechnung in Prozentpunkte (* 100)
      resids    = (afd_prop - pred_temp) * 100
    )
  
  # ---------------------------------------------------------
  # C. Plot erstellen
  # ---------------------------------------------------------
  plot_title <- paste0("Räumliche Residuen: ", region, " (", modelnummer, ")")
  
  # 1. Limits anpassen: +/- 15
  limit_val <- 5
  
  # 2. Graue Zone definieren: Alles zwischen -1 und +1 bleibt grau
  grey_threshold <- 1 
  
  color_limits <- c(-limit_val, limit_val)
  
  # 3. Ankerpunkte setzen (skaliert auf 0-1 für ggplot)
  color_values <- scales::rescale(c(-limit_val, -grey_threshold, grey_threshold, limit_val))
  
  # 4. Farben (Rot -> Grau -> Grau -> Blau)
  my_colors <- c("red", "grey80", "grey80", "blue")
  
  p_map <- ggplot(df_map) +
    geom_sf(aes(fill = resids), color = "white", size = 0.1) +
    
    scale_fill_gradientn(
      colours  = my_colors,
      values   = color_values,
      limits   = color_limits,
      oob      = scales::squish, # Alles über 15 wird dunkelblau/rot
      na.value = "grey90",
      name     = "Residuen in %"
    ) +
    
    labs(
      title = plot_title,
      subtitle = "Rot = Modell überschätzt AfD | Blau = Modell unterschätzt AfD"
    ) +
    
    theme_void() + 
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # ---------------------------------------------------------
  # D. Export & Return
  # ---------------------------------------------------------
  if (IS_PLOT_OUTPUT_ENABLED) {
    dateiname <- paste0("Karte_Residuen_", region, "_", modelnummer, ".png")
    speicherpfad <- file.path(EXPORT_PFAD_ABBILDUNGEN, dateiname)
    
    ggsave(filename = speicherpfad, plot = p_map, width = 12, height = 14, dpi = 300, bg = "white")
    message(paste0(">>> Karte gespeichert: ", speicherpfad))
  } else {
    message("--- Export von Abbildungen deaktiviert ---")
  }
  
  print(p_map)
  return(invisible(NULL))
}