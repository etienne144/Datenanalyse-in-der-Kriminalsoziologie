# DeskriptiveStatistik.R

# ************************************************************
# 1. Kernfunktion: Berechnung der univariaten deskriptiven Statistik
# ************************************************************
#' Berechnet die zentralen deskriptiven Statistiken (Mittelwert, Median, Streuung, Min/Max)
#' für eine einzelne metrische Variable.
#'
#' Diese Funktion wendet 'dplyr::summarise()' direkt auf eine Variable an, um deren
#' Verteilung in der Gesamtstichprobe zu beschreiben. Die Funktion arbeitet mit der
#' globalen Variable 'data'.
#'
#' @param column_name Die metrische Spalte, die analysiert werden soll. Wird mit Tidy Evaluation übergeben.
#' @return Ein Data Frame mit den berechneten deskriptiven Statistiken für die Spalte. Unsichtbare Rückgabe (invisible).
calculate_descriptive_stats <- function(column_name)
{
  # 0. Datenbereinigung (Geometrie weg)
  df_clean <- data
  if (inherits(df_clean, "sf")) {
    df_clean <- sf::st_drop_geometry(df_clean)
  }
  
  # 1. Erfassung des Spaltennamens als Klartext-String für die Ausgabe (Konsolenmeldung)
  column_name_str <- rlang::as_label(rlang::enquo(column_name))
  
  # 2. Berechnung der Kennzahlen mithilfe von dplyr::summarise()
  stats_result <- df_clean |>
    dplyr::summarise(
      Mittelwert = round(mean({{ column_name }}, na.rm = TRUE),NACHKOMMASTELLEN),
      Minimum = round(min({{ column_name }}, na.rm = TRUE),NACHKOMMASTELLEN),
      Quartil_25 = round(quantile({{ column_name }}, 0.25, na.rm = TRUE),NACHKOMMASTELLEN),
      Median = round(median({{ column_name }}, na.rm = TRUE),NACHKOMMASTELLEN),
      Quartil_75 = round(quantile({{ column_name }}, 0.75, na.rm = TRUE),NACHKOMMASTELLEN),
      Maximum = round(max({{ column_name }}, na.rm = TRUE),NACHKOMMASTELLEN),
      Std_Abweichung = round(sd({{ column_name }}, na.rm = TRUE),NACHKOMMASTELLEN),
      .groups = 'drop'
    )
  
  # 3. Steuerung der Ausgabe: Drucken der Einzelergebnisse, falls Ausgabe aktiviert
  if (IS_TABLE_OUTPUT_ENABLED)
  {
    cat("\nUNIVARIATE Analyse (Einzelvariable):", column_name_str, "\n")
    print(stats_result)
  }
  invisible(stats_result)
}

# ************************************************************
# 2. Iterationsfunktion: Führt die univariaten Analysen aus
# ************************************************************
#' Führt die univariate deskriptive Analyse für eine Liste von Variablen aus.
#'
#' Nutzt eine klassische 'for'-Schleife zur Iteration (anfängerfreundlich), ruft
#' 'calculate_descriptive_stats()' für jede Variable auf und fasst die Ergebnisse
#' am Ende in einem Data Frame zusammen.
#'
#' @param spalten_liste Vektor von Strings: Die metrischen Spalten, die analysiert werden sollen.
#' @return Ein zusammengefasster Data Frame mit den univariaten Ergebnissen aller analysierten Spalten.
#' @export
run_descriptive_analysis <- function(spalten_liste) 
{
  alle_ergebnisse <- list() # 1. Initialisierung: Liste für die Speicherung der Einzel-Ergebnis-DFs
  
  # 2. Iteration: Geht nacheinander alle Spaltennamen in der Liste durch
  for (i in seq_along(spalten_liste)) 
  {
    spalten_name <- spalten_liste[i]
    
    # Konvertierung des Strings in ein R-Objekt (Symbol) für die Übergabe an die Kern-Funktion
    spalte_als_symbol <- rlang::sym(spalten_name) 
    
    # Aufruf der Kern-Funktion
    stats_df <- calculate_descriptive_stats(column_name = !!spalte_als_symbol)
    
    alle_ergebnisse[[i]] <- stats_df # Speichert das Ergebnis in der Liste
  }
  
  # 3. Kombination: Die Liste der Data Frames zu einem einzigen Data Frame zusammenfügen
  all_stats_df <- do.call(rbind, alle_ergebnisse)
  
  # 4. Spaltennamen hinzufügen und umsortieren: Die Spalte 'Spalte' wird vorne eingefügt
  all_stats_df$Spalte <- spalten_liste
  all_stats_df <- all_stats_df[, c("Spalte", names(all_stats_df)[-length(names(all_stats_df))])]
  export_pfad <- paste0(EXPORT_PFAD_TABELLEN, "/Univariate.xlsx")
  # 5. Finale Ausgabe und Rückgabe: Anzeige nur, wenn die globale Steuerungsvariable TRUE ist
  if (IS_TABLE_OUTPUT_ENABLED) {
    dir.create(dirname(export_pfad), showWarnings = FALSE)
    
    # Export als Excel-Datei
    openxlsx::write.xlsx(
      all_stats_df, 
      file = export_pfad, 
      overwrite = TRUE, 
      sheetName = "Univariate Statistiken gesammt" # Definiert den Namen des einzigen Blattes
    )
    cat("\nZusammengefasste UNIVARIATE Analyse aller Spalten von Interesse\n")
    print(all_stats_df)
  }
  
  cat("\nUNIVARIATE Analyse abgeschlossen\n")
  return(all_stats_df)
}