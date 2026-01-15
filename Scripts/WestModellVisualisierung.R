#WestModellVisualisierung.R
visualize_west_diagnostics <- function(model_w, model_label) {
  
  # Prüfen, ob Visualisierung aktiviert ist
  if (!IS_PLOT_OUTPUT_ENABLED ) {
    message(paste0("--- Visualisierung für ", model_label, " übersprungen ---"))
    return(NULL)
  }
  
  # 1. Residuen-Vorbereitung mit DHARMa
  # Wir simulieren Residuen, um sie bei der Beta-Regression interpretierbar zu machen
  # im methodenteil unbedingt reflektieren
  s.resid <- DHARMa::simulateResiduals(model_w, seed = 1811, plot = FALSE)
  res_norm <- qnorm(residuals(s.resid)) # Transformation auf Normalverteilung
  pred <- predict(model_w)              # Erwartungswerte auf Link-Skala
  
  # ---------------------------------------------------------
  # PLOT 1: Residuals vs. Fitted
  # ---------------------------------------------------------
  df_plot <- data.frame(pred = pred, res = res_norm)
  
  p1 <- ggplot(df_plot, aes(x = pred, y = res)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", color = "blue", linewidth = 1.2) +
    geom_hline(yintercept = 0.0, color = "red", linewidth = 1) +
    theme_minimal() +
    labs(
      x = "Vorhergesagter AfD-Stimmanteil (fitted values)",
      y = "Quantil-Residuen (standardisiert)",
      title = paste("Residuals vs. Fitted:", model_label)
    )
  
  # Speichern
  ggsave(file.path(EXPORT_PFAD_ABBILDUNGEN, paste0("Diag_ResidFitted_", model_label, ".png")), 
         plot = p1, width = 20, height = 15, units = "cm", bg = "white")
  
  # ---------------------------------------------------------
  # PLOT 2: QQ-Plot
  # ---------------------------------------------------------
  qq_df <- data.frame(res_norm = res_norm)
  
  p2 <- ggplot(qq_df, aes(sample = res_norm)) +
    stat_qq(alpha = 0.5) +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_minimal() +
    labs(
      title = paste("QQ-Plot der Residuen:", model_label),
      x = "Theoretische Quantile",
      y = "Beobachtete Quantile"
    )
  
  ggsave(file.path(EXPORT_PFAD_ABBILDUNGEN, paste0("Diag_QQ_", model_label, ".png")), 
         plot = p2, width = 20, height = 15, units = "cm", bg = "white")
  
  # ---------------------------------------------------------
  # PLOT 3: Residuals vs. Prädiktoren (Partial Plots)
  # ---------------------------------------------------------
  
  # Wir holen uns die Namen der verwendeten Variablen direkt aus dem Modell
  # Wir filtern den Intercept und den Random Effect aus
  all_terms <- colnames(model.matrix(model_w))
  predictor_names <- all_terms[all_terms != "(Intercept)"]
  
  # Daten für den Plot zusammenbauen
  # Wir nutzen hier den Original-Datensatz aus dem Modell-Objekt
  df_res_data <- as.data.frame(model_w$frame)
  
  df_res_long <- df_res_data %>%
    dplyr::select(all_of(predictor_names)) %>%
    dplyr::mutate(res_norm = res_norm) %>%
    tidyr::pivot_longer(
      cols = all_of(predictor_names),
      names_to = "Predictor",
      values_to = "Value"
    )
  
  p3 <- ggplot(df_res_long, aes(x = Value, y = res_norm)) +
    geom_point(alpha = 0.4, size = 1) +
    geom_smooth(method = "loess", color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0, color = "red", linewidth = 1) +
    facet_wrap(~ Predictor, scales = "free_x") +
    theme_minimal() +
    labs(
      x = "Wert des Prädiktors",
      y = "Quantil-Residuen",
      title = paste("Residuen-Check pro Prädiktor:", model_label)
    )
  
  ggsave(file.path(EXPORT_PFAD_ABBILDUNGEN, paste0("Diag_Predictors_", model_label, ".png")), 
         plot = p3, width = 25, height = 20, units = "cm", bg = "white")
  
  message(paste0("--- Visualisierung ", model_label, " abgeschlossen ---"))
}