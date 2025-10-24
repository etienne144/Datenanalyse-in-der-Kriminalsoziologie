# DeskriptiveStatistik.R
# ************************************************************
# 1. Funktion für EINZELNE Spalten (Core-Logik)
# ************************************************************
calculate_descriptive_stats <- function(column_name)
{
  # 1. Spaltennamen als String für die Ausgabe erfassen
  column_name_str <- rlang::as_label(rlang::enquo(column_name))
  
  # 2. Berechnung durchführen und Ergebnis zuweisen
  stats_result <- data |>
    dplyr::summarise(
      Mean = mean({{ column_name }}, na.rm = TRUE),
      Min = min({{ column_name }}, na.rm = TRUE),
      Quartil25 = quantile({{ column_name }}, 0.25, na.rm = TRUE),
      Median = median({{ column_name }}, na.rm = TRUE),
      Quartil75 = quantile({{ column_name }}, 0.75, na.rm = TRUE),
      Max = max({{ column_name }}, na.rm = TRUE),
      SD = sd({{ column_name }}, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # 3. Steuerung der Ausgabe
  if (PRINT_RESULTS_SINGLE)
  {
    cat("\nUNIVARIATE Analyse:", column_name_str, "\n") 
    print(stats_result)
  }
  invisible(stats_result)
}
# ************************************************************
# 2. Master-Funktion für die Iteration
# ************************************************************
run_descriptive_analysis <- function(spalten_liste) {
  
  # 1. Iteration über die Liste der "spalten_von_interesse" aus MAIN.R
  deskriptive_ergebnisse <- spalten_liste |>
    purrr::map(
      .f = ~ calculate_descriptive_stats(column_name = !!rlang::sym(.x))
    )
  # 2. Die Liste zu einem einzigen, sauberen Data Frame kombinieren
  all_stats_df <- deskriptive_ergebnisse |>
    purrr::list_rbind() |>
    dplyr::mutate(Partei = spalten_liste, .before = 1) 
  
  # 3. Finale zusammengefasste Tabelle anzeigen
  if (PRINT_RESULTS_TOTAL) {
    cat("\nZusammengefasste deskriptive Statistik aller Spalten\n")
    print(all_stats_df)
  }
  
  # 4. MAIN.R erhält die zusammengefasste Tabelle
  return(all_stats_df)
}