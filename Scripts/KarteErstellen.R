# ==============================================================================
# Funktion: plot_residual_map (FINAL & FLEXIBEL)
# ==============================================================================
# ZWECK:
# Visualisiert die räumliche Verteilung der Modellfehler (Residuen).
#
# NEUERUNG (WICHTIG):
# Die Funktion akzeptiert jetzt ein optionales Argument 'west_model'.
#
# 1. Nutzung OHNE west_model:
#    - Standardfall. Wir testen, wie gut ein Modell "aus sich heraus" vorhersagt.
#    - Wird genutzt für den harten Ost-West-Vergleich (Struktur-Check).
#
# 2. Nutzung MIT west_model:
#    - Spezialfall für Transfermodelle.
#    - Da unsere Ost-Modelle einen fixen "Offset" (die West-Steigung) brauchen,
#      übergeben wir hier das West-Modell als "Anker".
#    - Die Funktion berechnet dann erst den Offset und dann die Binnen-Vorhersage.
# ==============================================================================

plot_residual_map <- function(model, region, modelnummer, west_model = NULL) {
  
  message(paste0(">>> Starte Karten-Erstellung für: ", modelnummer, " (Region: ", region, ")"))
  
  # ---------------------------------------------------------
  # A. Daten filtern (Region auswählen)
  # ---------------------------------------------------------
  # Wir prüfen, ob wir ganz Deutschland oder nur Ost/West wollen.
  
  is_gesamt <- grepl("gesamt", tolower(region))
  
  if (is_gesamt) {
    # Fall 1: Gesamtdeutschland
    df_map <- data 
  } else {
    # Fall 2: Ost oder West (0 oder 1)
    target_ost <- if (grepl("west", tolower(region))) 0 else 1
    df_map <- data %>% dplyr::filter(ost == target_ost)
  }
  
  # ---------------------------------------------------------
  # B. Residuen berechnen (Hier ist die neue Logik!)
  # ---------------------------------------------------------
  
  # SCHRITT B1: Offset berechnen (Nur nötig für Binnen-Modelle)
  # Wenn wir das Transfer-Modell (mit 1|state) plotten wollen, braucht es
  # zwingend die Information, was das West-Modell "denkt" (den Linear Predictor).
  if (!is.null(west_model)) {
    message("    -> Info: Berechne Offset-Werte aus dem West-Modell...")
    df_map$linear_predictor <- predict(west_model, newdata = df_map, type = "link", allow.new.levels = TRUE)
  }
  
  # SCHRITT B2: Die eigentliche Vorhersage und Fehlerberechnung
  df_map <- df_map %>%
    dplyr::mutate(
      # Vorhersage des Modells (in % Wahrscheinlichkeit)
      # predict() ist schlau: Wenn 'linear_predictor' existiert (siehe oben),
      # nutzt es diesen automatisch für den Offset.
      pred_temp = predict(model, newdata = ., type = "response", allow.new.levels = TRUE),
      
      # Residuum berechnen: Realität minus Vorhersage
      # Beispiel: Real 20% - Vorhersage 10% = +10 (Positiv)
      # Positiv (+) -> AfD stärker als gedacht -> BLAU (Unterschätzung)
      # Negativ (-) -> AfD schwächer als gedacht -> ROT (Überschätzung)
      resids    = (afd_prop - pred_temp) * 100
    )
  
  # ---------------------------------------------------------
  # C. Plot erstellen (ggplot2)
  # ---------------------------------------------------------
  plot_title <- paste0("Räumliche Residuen: ", region, " (", modelnummer, ")")
  
  # Grenzen für die Farbskala festlegen (+/- 15 Prozentpunkte)
  limit_val <- 15
  
  # Graue Zone definieren (kleine Fehler +/- 0.5% bleiben grau)
  grey_threshold <- 0.5 
  
  # Skalierung vorbereiten
  color_limits <- c(-limit_val, limit_val)
  color_values <- scales::rescale(c(-limit_val, -grey_threshold, grey_threshold, limit_val))
  
  # Farben definieren (Rot = Überschätzt, Blau = Unterschätzt)
  my_colors <- c("red", "grey80", "grey80", "blue")
  
  p_map <- ggplot2::ggplot(df_map) +
    # Die Karte zeichnen
    ggplot2::geom_sf(ggplot2::aes(fill = resids), color = "white", lwd = 0.1) +
    
    # Die Farben anwenden
    ggplot2::scale_fill_gradientn(
      colours  = my_colors,
      values   = color_values,
      limits   = color_limits,
      oob      = scales::squish, # Alles über 15 wird dunkelblau/rot gekappt
      na.value = "grey90",
      name     = "Residuen in %"
    ) +
    
    # Beschriftung
    ggplot2::labs(
      title = plot_title,
      caption  = paste0("Modell: ", modelnummer)
    ) +
    
    # Layout bereinigen (keine Achsen, weißer Hintergrund)
    ggplot2::theme_void() + 
    ggplot2::theme(
      legend.position = "right",
      plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, color = "grey40", size = 9)
    )
  
  # ---------------------------------------------------------
  # D. Export & Ausgabe
  # ---------------------------------------------------------
  if (exists("IS_PLOT_OUTPUT_ENABLED") && IS_PLOT_OUTPUT_ENABLED) {
    dateiname <- paste0("Karte_Residuen_", region, "_", modelnummer, ".png")
    speicherpfad <- file.path(EXPORT_PFAD_ABBILDUNGEN, dateiname)
    
    # Speichern in hoher Qualität
    ggplot2::ggsave(filename = speicherpfad, plot = p_map, width = 10, height = 8, dpi = 300, bg = "white")
    message(paste0("    -> Karte gespeichert unter: ", speicherpfad))
  }
  
  print(p_map)
  return(invisible(NULL))
}