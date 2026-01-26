# ==============================================================================
# Funktion: Residuen berechnen und exportieren
# ==============================================================================
# Diese Funktion führt folgende Schritte aus:
# 1. Filtert den Datensatz auf die gewünschte Region (West oder Ost).
# 2. Berechnet die Vorhersagen des Modells und die Residuen (Fehler).
# 3. Erstellt statistische Kennzahlen (Quantile, Mittelwerte).
# 4. Speichert diese Statistiken direkt als Excel-Datei ab.
# ==============================================================================

calculate_and_export_residuals <- function(model, region, modelnummer) {
  
  # ---------------------------------------------------------
  # A. Vorbereitung
  # ---------------------------------------------------------
  
  # 0. Region bestimmen
  # Wir prüfen, ob im Text "west" steht. Falls ja -> 0, sonst -> 1 (für Ost).
  # tolower macht den Text klein, damit "West" und "west" erkannt wird.
  target_ost <- if (grepl("west", tolower(region))) 0 else 1
  
  # 1. Datensatz filtern
  # Wir arbeiten nur mit den Zeilen, die zur Region gehören.
  # Das verhindert, dass wir Ost-Daten versehentlich mit West-Logik mischen
  # (außer wir wollen das explizit beim Transfer).
  df_work <- data %>%
    dplyr::filter(ost == target_ost)
  
  # Geometrie entfernen
  # Falls es sich um Geodaten (sf) handelt, entfernen wir die Karte,
  # da das Rechnen sonst unnötig langsam wird.
  if (inherits(df_work, "sf")) df_work <- sf::st_drop_geometry(df_work)
  
  # ---------------------------------------------------------
  # B. Berechnung der Residuen
  # ---------------------------------------------------------
  
  # Wir bauen die Spaltennamen dynamisch zusammen (z.B. "pred_west_m1")
  col_pred  <- paste0("pred_", region, "_", modelnummer)
  col_resid <- paste0("resid_", region, "_", modelnummer)
  
  message(paste0(">>> Berechne Residuen für: ", col_resid))
  
  # Berechnung durchführen
  # !!sym() wandelt den Text-String "col_pred"
  # in einen echten Spaltennamen um, damit mutate ihn versteht.
  df_work <- df_work %>%
    dplyr::mutate(
      !!sym(col_pred)  := predict(model, newdata = ., type = "response"),
      !!sym(col_resid) := afd_prop - !!sym(col_pred) # Realität minus Vorhersage
    )
  
  # ---------------------------------------------------------
  # C. Export der Ergebnisse
  # ---------------------------------------------------------
  

  # Falls die Variable IS_TABLE_OUTPUT_ENABLED  FALSE ist, brechen wir ab.
  if (!IS_TABLE_OUTPUT_ENABLED) {
    message("--- Export von Tabellen deaktiviert ---")
    return(invisible(NULL))
  }
  
  # Wir ziehen uns die berechneten Residuen in eine eigene Variable
  res_values <- df_work[[col_resid]]
  
  # Tabelle 1: Quantile (Verteilung der Fehler in 10%-Schritten)
  stats_quantiles <- data.frame(
    Quantil = paste0(seq(0, 100, 10), "%"),
    Wert    = round(quantile(res_values, probs = seq(0, 1, 0.10), na.rm = TRUE), NACHKOMMASTELLEN +1)
  )
  
  # Tabelle 2: Summary (Standard-Kennzahlen wie Min, Max, Median, Mean)
  sum_obj <- summary(res_values)
  stats_summary <- data.frame(
    Kennzahl = names(sum_obj),
    Wert     = as.numeric(sum_obj)
  )
  
  # Erstellung einer Liste für die Excel-Blätter (jedes Element wird ein Tabellenblatt)
  output_list <- list(
    "Quantile" = stats_quantiles,
    "Summary"  = stats_summary
  )
  
  # Dateipfad zusammenbauen und speichern
  dateiname <- paste0("Residuen_Analyse_", region, "_", modelnummer, ".xlsx")
  export_full_path <- file.path(EXPORT_PFAD_TABELLEN, dateiname)
  
  openxlsx::write.xlsx(output_list, file = export_full_path, overwrite = TRUE)
  message(paste0(">>> Excel erfolgreich gespeichert: ", export_full_path))
  
  # invisible(NULL) bedeutet: Die Funktion ist fertig, gibt aber kein Ergebnis
  # an die Konsole zurück (verhindert Datenmüll im Output).
  return(invisible(NULL))
}