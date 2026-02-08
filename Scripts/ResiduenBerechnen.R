# ==============================================================================
# Funktion: Residuen berechnen und exportieren
# ==============================================================================
# Diese Funktion führt folgende Schritte aus:
# 1. Filtert den Datensatz auf die gewünschte Region (West oder Ost).
# 2. Berechnet die Vorhersagen des Modells und die Residuen (Fehler).
# 3. Erstellt statistische Kennzahlen (Quantile, Mittelwerte).
# 4. Speichert diese Statistiken direkt als Excel-Datei ab.
# ==============================================================================
calculate_and_export_residuals <- function(model, region, modelnummer, west_model = NULL) {
  
  # ---------------------------------------------------------
  # A. Vorbereitung
  # ---------------------------------------------------------
  
  # 0. Region bestimmen
  target_ost <- if (grepl("west", tolower(region))) 0 else 1
  
  # 1. Datensatz filtern
  df_work <- data %>%
    dplyr::filter(ost == target_ost)
  
  # Geometrie entfernen (falls sf-Objekt)
  if (inherits(df_work, "sf")) df_work <- sf::st_drop_geometry(df_work)
  
  # ---------------------------------------------------------
  # B. Berechnung der Residuen (Hier ist das UPDATE!)
  # ---------------------------------------------------------
  
  # Wenn ein West-Modell übergeben wurde, berechnen wir hier den "Anker".
  if (!is.null(west_model)) {
    message("   -> Info: Nutze West-Modell als Offset-Basis (Binnen-Sicht).")
    # Wir fügen die Spalte hinzu, die das Transfer-Modell braucht
    df_work$linear_predictor <- predict(west_model, newdata = df_work, type = "link", allow.new.levels = TRUE)
  }
  
  # Wir bauen die Spaltennamen dynamisch zusammen
  col_pred  <- paste0("pred_", region, "_", modelnummer)
  col_resid <- paste0("resid_", region, "_", modelnummer)
  
  message(paste0("--- Berechne Residuen für: ", col_resid)," ---")
  
  # Berechnung durchführen
  # predict() nutzt jetzt automatisch den Offset ('linear_predictor'), 
  # falls wir ihn oben eingefügt haben.
  df_work <- df_work %>%
    dplyr::mutate(
      !!rlang::sym(col_pred)  := predict(model, newdata = ., type = "response", allow.new.levels = TRUE),
      !!rlang::sym(col_resid) := afd_prop - !!rlang::sym(col_pred) # Realität - Vorhersage
    )
  
  # ---------------------------------------------------------
  # C. Export der Ergebnisse (Dein Original-Code)
  # ---------------------------------------------------------
  
  if (!IS_TABLE_OUTPUT_ENABLED) {
    message("--- Export von Tabellen deaktiviert ---")
    return(invisible(NULL))
  }
  
  # Wir ziehen uns die berechneten Residuen
  res_values <- df_work[[col_resid]]
  
  # Tabelle 1: Quantile
  stats_quantiles <- data.frame(
    Quantil = paste0(seq(0, 100, 10), "%"),
    Wert    = round(quantile(res_values, probs = seq(0, 1, 0.10), na.rm = TRUE), NACHKOMMASTELLEN + 1)
  )
  
  # Tabelle 2: Summary
  sum_obj <- summary(res_values)
  stats_summary <- data.frame(
    Kennzahl = names(sum_obj),
    Wert     = as.numeric(sum_obj)
  )
  
  # Liste für Excel
  output_list <- list(
    "Quantile" = stats_quantiles,
    "Summary"  = stats_summary
  )
  
  # Speichern
  dateiname <- paste0("Residuen_Analyse_", region, "_", modelnummer, ".xlsx")
  export_full_path <- file.path(EXPORT_PFAD_TABELLEN, dateiname)
  openxlsx::write.xlsx(output_list, file = export_full_path, overwrite = TRUE)
  
  return(invisible(NULL))
}