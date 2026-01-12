  # BivariateDeskriptiveStatistik.R
  
  # ************************************************************
  # 1. Kernfunktion: Berechnung des Gruppenvergleichs
  # ************************************************************
  #' Berechnet die zentralen deskriptiven Statistiken (Mittelwert, Median, Streuung)
  #' für eine metrische Variable, aufgeteilt nach den Kategorien einer nominalen/ordinalen Variable.
  #'
  #' Diese Funktion nutzt die Logik "Teilen-Anwenden-Zusammenfassen" (Split-Apply-Combine),
  #' um die Kennzahlen für jede Untergruppe (z.B. Ost und West) separat zu berechnen.
  #' Die Funktion greift auf den globalen Datensatz 'data' zu.
  #'
  #' @param target_column_name String: Name der metrischen Zielspalte, die analysiert wird (z.B. "Einkommen").
  #' @param grouping_column_name String: Name der kategorialen Spalte, nach der gruppiert wird (z.B. "Ost_West").
  #' @return Ein Data Frame, der die zentralen Kennzahlen für jede Gruppe separat ausweist.
  #' @export
calculate_bivariate_stats <- function(target_column_name, grouping_column_name) {
  
  # 1. Datenbereinigung (Geometrie weg)
  df_clean <- data
  if (inherits(df_clean, "sf")) {
    df_clean <- sf::st_drop_geometry(df_clean)
  }
  
  # 2. Symbole erstellen (WICHTIG: sicherstellen, dass sie existieren)
  target_sym <- rlang::sym(target_column_name)
  group_sym  <- rlang::sym(grouping_column_name)
  
  # 3. Berechnung
  # Hinweis: Wir nutzen hier explizit df_clean
  stats_result <- df_clean %>%
    dplyr::group_by(!!group_sym) %>%
    dplyr::summarise(
      Mittelwert      = round(mean(!!target_sym, na.rm = TRUE), NACHKOMMASTELLEN),
      Minimum         = round(min(!!target_sym, na.rm = TRUE), NACHKOMMASTELLEN),
      Quartil_25      = round(as.numeric(quantile(!!target_sym, 0.25, na.rm = TRUE)), NACHKOMMASTELLEN),
      Median          = round(median(!!target_sym, na.rm = TRUE), NACHKOMMASTELLEN),
      Quartil_75      = round(as.numeric(quantile(!!target_sym, 0.75, na.rm = TRUE)), NACHKOMMASTELLEN),
      Maximum         = round(max(!!target_sym, na.rm = TRUE), NACHKOMMASTELLEN),
      Std_Abweichung  = round(sd(!!target_sym, na.rm = TRUE), NACHKOMMASTELLEN),
      Fallzahl_n      = dplyr::n(),
      .groups = 'drop'
    )
  
  # 4. Variable als Spalte hinzufügen
  stats_result$Variable <- target_column_name
  
  # 5. Rückgabe
  return(stats_result)
}
  
  # ************************************************************
  # 2. Iterationsfunktion: Führt eine Gruppe von bivariaten Analysen aus
  # ************************************************************
  #' Führt die bivariate Analyse für mehrere Zielvariablen gegen EINE Gruppierungsvariable aus.
  #'
  #' Nutzt eine klassische 'for'-Schleife zur Iteration (anfängerfreundlich) und
  #' stapelt die Ergebnisse der Einzelberechnungen am Ende in einem Data Frame.
  #'
  #' @param grouping_variable String: Die kategoriale Spalte, die für alle Zielvariablen als Gruppe dient.
  #' @param target_variables Vektor von Strings: Die metrischen Spalten, die analysiert werden sollen.
  #' @return Ein zusammengefasster Data Frame mit den Ergebnissen aller Zielvariablen.
  #' @export
  run_bivariate_analysis <- function(grouping_variable, target_variables)
  {
    # 1. Initialisierung: Leere Liste für die Speicherung der Data Frames aus jedem Schleifendurchlauf
    alle_ergebnisse <- list() 
    
    # 2. Iteration: Geht nacheinander alle Zielvariablen durch
    for (i in seq_along(target_variables)) 
    {
      target_name <- target_variables[i]
      
      # Aufruf der Kern-Funktion für die aktuelle Kombination (Gruppe vs. Zielvariable)
      stats_df <- calculate_bivariate_stats(
        target_column_name = target_name, 
        grouping_column_name = grouping_variable
      )
      
      # Ergebnis in die Liste einfügen
      alle_ergebnisse[[i]] <- stats_df 
    }
    
    # 3. Kombination: Fügt die einzelnen Ergebnis-Data Frames zeilenweise zusammen ('rbind')
    all_stats_df <- do.call(rbind, alle_ergebnisse)
    
    # 4. Spaltenreihenfolge anpassen: 'Variable' an den Anfang verschieben (für bessere Lesbarkeit)
    all_stats_df <- all_stats_df |>
      dplyr::select(Variable, dplyr::everything())
    
    # 5. Finale Ausgabe und Rückgabe: Anzeige nur, wenn die globale Steuerungsvariable (IS_TABLE_OUTPUT_ENABLED) TRUE ist
    if (IS_TABLE_OUTPUT_ENABLED) {
      print(all_stats_df)
    }
    return(all_stats_df)
  }
  
  # ************************************************************
  # 3. Wrapper-Funktion: Führt ALLE definierten Analysen aus
  # ************************************************************
  #' Führt mehrere, unabhängige bivariate Analysen aus.
  #'
  #' Diese Funktion iteriert über eine Master-Liste von Analyse-Definitionen, die im
  #' Hauptskript festgelegt wurden. Sie ermöglicht den einfachen Wechsel von
  #' Gruppierungsvariablen und Zielvariablen von Analyse zu Analyse.
  #'
  #' @param analysen_liste Eine Liste von Listen, die jede Analyse definiert (grouping_variable und target_variables).
  #' @return Eine benannte Liste von Data Frames, wobei jeder DF die Ergebnisse einer kompletten Analyse enthält.
  #' @export
  run_multiple_bivariate_analysis <- function(analysen_liste)
  {
    all_results <- list()
    export_pfad_final <- paste0(EXPORT_PFAD_TABELLEN, "/BIvariate.xlsx")
    # 1. Iteriere über jede benannte Analyse (z.B. "Analyse_Bildung_OstWest")
    for (analyse_name in names(analysen_liste)) {
      
      aktuelle_analyse <- analysen_liste[[analyse_name]]
      
      # 2. Extrahiere die spezifischen Parameter für diese Analyse
      grouping_var <- aktuelle_analyse$grouping_variable
      target_vars <- aktuelle_analyse$target_variables
      
      # Statusmeldung (Output)
      cat(paste0("\n--- Starte Analyse: ", analyse_name, 
                 " (Gruppe: ", grouping_var, ") ---\n"))
      
      # 3. Führe die Analyse (Iteration) aus
      ergebnis_df <- run_bivariate_analysis(
        grouping_variable = grouping_var,
        target_variables = target_vars
      )
      
      # 4. Speichere das vollständige Ergebnis unter dem Analysen-Namen in der Ergebnisliste
      all_results[[substr(analyse_name, 1, 31)]] <- ergebnis_df
    }
    dir.create(dirname(export_pfad_final), showWarnings = FALSE)
    
    # 3. Export: Die Liste 'all_results' wird in eine EINZELNE Excel-Datei geschrieben
    # Jeder Listenname (z.B. 'des_Bildungstand') wird automatisch zum Blattnamen.
    openxlsx::write.xlsx(
      all_results, 
      file = export_pfad_final, 
      overwrite = TRUE
    )
    cat("\nAlle Bivariaten Analysen abgeschlossen.\n")
    return(all_results)
  }