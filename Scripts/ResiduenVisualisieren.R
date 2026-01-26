# ==============================================================================
# Funktion: Histogramm der Residuen erstellen und speichern
# ==============================================================================
# Diese Funktion visualisiert die Verteilung der Modellfehler (Residuen).
# Schritte:
# 1. Filtert Daten auf die Zielregion (West oder Ost).
# 2. Berechnet die Residuen (Differenz zwischen Realität und Modell).
# 3. Erstellt ein Histogramm mit ggplot2.
# 4. Speichert die Grafik als PNG, falls der Export aktiviert ist.
# ==============================================================================

plot_residual_histogram <- function(model, region, modelnummer) {
  
  # ---------------------------------------------------------
  # A. Daten vorbereiten
  # ---------------------------------------------------------
  
  # 0. Region bestimmen
  # Wir prüfen, ob im Text "west" steht. Falls ja -> 0, sonst -> 1 (für Ost).
  target_ost <- if (grepl("west", tolower(region))) 0 else 1
  
  # 1. Datensatz filtern & Geometrie entfernen
  # Wir nutzen data als Argument (sauberer Stil), filtern auf die Region
  # und entfernen Geometrie für Performance.
  df_plot <- data %>%
    dplyr::filter(ost == target_ost)
  
  if (inherits(df_plot, "sf")) df_plot <- sf::st_drop_geometry(df_plot)
  
  # 2. Residuen berechnen
  # Temporäre Berechnung für den Plot
  df_plot <- df_plot %>%
    dplyr::mutate(
      pred_temp = predict(model, newdata = ., type = "response"),
      resids    = (afd_prop - pred_temp) * 100
    )
  
  # ---------------------------------------------------------
  # B. Plot erstellen
  # ---------------------------------------------------------
  
  # Dynamischer Titel
  plot_title  <- paste0("Residuen ", modelnummer, " ", region)
  
  # WICHTIG: Variable 'p' nennen, nicht 'plot' (Konflikt mit Base-R)
  p <- ggplot(df_plot, aes(x = resids)) +
    geom_histogram(binwidth = 1, fill = "#6291eb", color = "white", alpha = 0.8) +
    
    # Rote Linie bei 0 (Perfekte Vorhersage)
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    
    labs(
      title = plot_title,
      x = "Residuen in Prozentpunkten",
      y = "Anzahl Wahlkreise"
    ) +
    theme_minimal()
  
  # ---------------------------------------------------------
  # C. Speichern (Export)
  # ---------------------------------------------------------
  
  # Prüfung: Ist der Bilder-Export aktiviert?
  if (IS_PLOT_OUTPUT_ENABLED) {
    
    # Dateinamen generieren
    dateiname <- paste0("Histogramm_Residuen_", region, "_", modelnummer, ".png")
    speicherpfad <- file.path(EXPORT_PFAD_ABBILDUNGEN, dateiname)
    
    # Speichern
    ggsave(filename = speicherpfad, plot = p, width = 8, height = 6, dpi = 300)
    
    message(paste0(">>> Histogramm gespeichert: ", speicherpfad))
    
  } else {
    message("--- Export von Abbildungen deaktiviert ---")
  }
  
  # ---------------------------------------------------------
  # D. Anzeige und Return
  # ---------------------------------------------------------
  
  # Plot im RStudio-Fenster anzeigen
  print(p)
  
  # "Unsichtbare NULL" zurückgeben
  return(invisible(NULL))
}