# ==============================================================================
# Funktion: plot_residual_histogram (FINAL & FLEXIBEL)
# ==============================================================================
# UPDATE: 
# Akzeptiert jetzt 'west_model' als optionales Argument.
# Das ist notwendig, um Histogramme für Transfer-Modelle (mit Offset) zu erstellen.
# ==============================================================================

plot_residual_histogram <- function(model, region, modelnummer, west_model = NULL) {
  
  # ---------------------------------------------------------
  # A. Daten vorbereiten
  # ---------------------------------------------------------
  
  # 0. Region bestimmen
  target_ost <- if (grepl("west", tolower(region))) 0 else 1
  
  # 1. Datensatz filtern
  df_plot <- data %>%
    dplyr::filter(ost == target_ost)
  
  # Geometrie entfernen (Wichtig für Performance bei ggplot histograms)
  if (inherits(df_plot, "sf")) df_plot <- sf::st_drop_geometry(df_plot)
  
  # ---------------------------------------------------------
  # B. Residuen berechnen (Hier ist der FIX!)
  # ---------------------------------------------------------
  
  # SCHRITT B1: Offset berechnen (Nur für Binnen-Modelle nötig)
  if (!is.null(west_model)) {
    message("   -> Info: Nutze West-Modell als Offset-Basis für Histogramm.")
    # Wir berechnen den Anker-Wert (linear_predictor)
    df_plot$linear_predictor <- predict(west_model, newdata = df_plot, type = "link", allow.new.levels = TRUE)
  }
  
  # SCHRITT B2: Die eigentliche Vorhersage
  # Wir nutzen 'allow.new.levels = TRUE', damit auch Gap-Checks (West auf Ost) nicht crashen
  df_plot <- df_plot %>%
    dplyr::mutate(
      pred_temp = predict(model, newdata = ., type = "response", allow.new.levels = TRUE),
      resids    = (afd_prop - pred_temp) * 100
    )
  
  # ---------------------------------------------------------
  # C. Plot erstellen
  # ---------------------------------------------------------
  
  plot_title  <- paste0("Verteilung der Residuen: ", modelnummer, " (", region, ")")
  
  p <- ggplot(df_plot, aes(x = resids)) +
    # Histogramm: Farbe Blau, halbtransparent
    geom_histogram(binwidth = 1, fill = "#6291eb", color = "white", alpha = 0.8) +
    
    # Rote Linie bei 0 (Perfekte Vorhersage)
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
    
    labs(
      title = plot_title,
      subtitle = "Rechts von 0 = Modell unterschätzt AfD | Links von 0 = Modell überschätzt AfD",
      x = "Abweichung in Prozentpunkten",
      y = "Anzahl Wahlkreise"
    ) +
    theme_minimal()
  
  # ---------------------------------------------------------
  # D. Speichern (Export)
  # ---------------------------------------------------------
  
  if (IS_PLOT_OUTPUT_ENABLED) {
    dateiname <- paste0("Histogramm_Residuen_", region, "_", modelnummer, ".png")
    speicherpfad <- file.path(EXPORT_PFAD_ABBILDUNGEN, dateiname)
    
    ggsave(filename = speicherpfad, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
    message(paste0("--- Histogramm gespeichert: ", modelnummer)," ---")
  } else {
    message("--- Export deaktiviert ---")
  }
  
  print(p)
  return(invisible(NULL))
}