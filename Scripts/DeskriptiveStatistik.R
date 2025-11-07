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
run_descriptive_analysis <- function(spalten_liste) 
{
  alle_ergebnisse <- list() # Für die Speicherung der Ergebnisse
  
  # 1. Iteration über die Liste der "spalten_von_interesse" aus MAIN.R
  for (i in seq_along(spalten_liste)) 
  {
    spalten_name <- spalten_liste[i]
    spalte_als_symbol <- rlang::sym(spalten_name) 
    stats_df <- calculate_descriptive_stats(column_name = !!spalte_als_symbol)
    alle_ergebnisse[[i]] <- stats_df # Ergebnisse in die Liste einfügen
  }
  # 2. Die Liste zu einem einzigen, Data Frame kombinieren
  all_stats_df <- do.call(rbind, alle_ergebnisse)
  
  # 3. Spaltennamen hinzufügen
  all_stats_df$Spalte <- spalten_liste
  all_stats_df <- all_stats_df[, c("Spalte", names(all_stats_df)[-length(names(all_stats_df))])]
  
  # 4. Finale zusammengefasste Tabelle anzeigen (prüft global PRINT_RESULTS)
  if (PRINT_RESULTS_TOTAL) {
    cat("\nZusammengefasste UNIVARIATE Analyse aller Spalten von interesse\n")
    print(all_stats_df)
  }
  
  # 5. MAIN.R erhält die zusammengefasste Tabelle
  cat("\nUNIVARIATE Analyse abgeschlossen\n")
  return(all_stats_df)
}
# ************************************************************
# 3. Funktion für BIVARIATE
# ************************************************************
#neu muss noch überarbeitet werden

calculate_bivariat_descriptive_stats <- function(column_name, group_by_var)
{
  # 1. Spaltennamen als String für die Ausgabe erfassen
  column_name_str <- rlang::as_label(rlang::enquo(column_name))
  group_by_str <- rlang::as_label(rlang::enquo(group_by_var))
  
  # 2. Berechnung durchführen und Ergebnis zuweisen
  stats_result <- data |>
    dplyr::group_by({{ group_by_var }}) |>
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
    cat("\nBIVARIATE Analyse:", column_name_str, " gruppiert nach ", group_by_str,"\n") 
    print(stats_result)
  }
  invisible(stats_result)
}

calculate_quartile_descriptive_stats <- function(column_name, quartile_var)
{
  # 1. Spaltennamen als String für die Ausgabe erfassen
  column_name_str <- rlang::as_label(rlang::enquo(column_name))
  quartile_var_str <- rlang::as_label(rlang::enquo(quartile_var))
  
  # --- DATENVORBEREITUNG ---
  # Erstelle Data Frame mit der Quartilgruppe (FÜR PLOT UND SUMMARISE)
  data_gegruppert <- data |> 
    dplyr::mutate(QuartilGruppe = dplyr::ntile({{ quartile_var }}, n = 4))
  
  # --- PLOT-GENERIERUNG (NEU) ---
  if (exists("PRINT_PLOT_RESULTS") && PRINT_PLOT_RESULTS) {
    # Erstelle Boxplot, das die Rohdaten aus 'data_gegruppert' nutzt
    boxplot_plot <- ggplot2::ggplot(data_gegruppert, 
                                    ggplot2::aes(x = factor(QuartilGruppe), 
                                                 y = {{ column_name }})) + # Nutzt Tidy-Eval für die Y-Achse
      ggplot2::geom_boxplot(fill = "salmon", alpha = 0.6) +
      ggplot2::labs(
        title = paste0("Verteilung von ", column_name_str, " nach Quartilen von ", quartile_var_str),
        x = paste0("Quartilsgruppe: ", quartile_var_str, " (1 = niedrigster Wert)"),
        y = column_name_str
      ) +
      ggplot2::theme_minimal()
    
    # Die Grafik direkt anzeigen
    print(boxplot_plot)
  }
  
  # --- STATISTIK-BERECHNUNG ---
  # Berechnung basiert nun auf dem temporären Data Frame
  stats_result <- data_gegruppert |>
    dplyr::group_by(QuartilGruppe) |> # Nutzt die bereits erstellte Spalte
    
    dplyr::summarise(
      N = dplyr::n(), 
      # ... (restliche Statistik wie gehabt) ...
      Mean = mean({{ column_name }}, na.rm = TRUE),
      Median = median({{ column_name }}, 0.50, na.rm = TRUE),
      SD = sd({{ column_name }}, na.rm = TRUE),
      Min = min({{ column_name }}, na.rm = TRUE),
      Quartil25 = quantile({{ column_name }}, 0.25, na.rm = TRUE),
      Quartil75 = quantile({{ column_name }}, 0.75, na.rm = TRUE),
      Max = max({{ column_name }}, na.rm = TRUE),
      Mean_Quartil_Var = mean({{ quartile_var }}, na.rm = TRUE), 
      .groups = 'drop' 
    )
  
  # 3. Steuerung der Tabellen-Ausgabe
  if (exists("PRINT_RESULTS_SINGLE") && PRINT_RESULTS_SINGLE)
  {
    cat("\n--- BIVARIATE Analyse: ", column_name_str, " gruppiert nach Quartilen von ", quartile_var_str, " ---\n") 
    print(stats_result)
  }
  invisible(stats_result)
}