# DeskriptiveStatistikVisualisierung.R

# ************************************************************
# 1. Funktion für EINZELNE Spalten (Core-Logik)
# ************************************************************
plot_univariate_histogram <- function(column_name)
{
    # 1. Spaltennamen als String für die Ausgabe erfassen
    column_name_str <- rlang::as_label(rlang::enquo(column_name))
    
    # 2. Den Wertebereich ermitteln und somit die binweite bestimmen
    daten_vektor <- data |> dplyr::pull({{ column_name }})
    anzahl_bins <- nclass.Sturges(daten_vektor)
    
    #3. Das Histogram erstellen
    histogram_plot <- ggplot2::ggplot(data, 
                                      ggplot2::aes(x = {{ column_name }})) +
      
      ggplot2::geom_histogram(bins = anzahl_bins * HISTOGRAM_BINS_FAKTOR, 
                              fill = "skyblue", 
                              color = "black", 
                              alpha = 0.7) +
      
      ggplot2::labs(
        x = column_name_str,
        y = "Anzahl Beobachtungen",
        title = paste0("Verteilung von: ", column_name_str)
      ) +
      ggplot2::theme_minimal()
    
    print(histogram_plot)
  
    invisible(NULL)
}

# ************************************************************
# 2. Master-Funktion für die Iteration
# ************************************************************
run_univariate_plotting <- function(spalten_liste)
{
  # Läuft nur wenn PRINT_PLOT_RESULTS
  if (!(exists("PRINT_PLOT_RESULTS") && PLOT_RESULTS)) {
    cat("PRINT_PLOT_RESULTS ist FALSE. Visualisierung wurde uebersprungen.")
    return(invisible(NULL))
  }
  
  # Iteration über die Spaltenliste
  for (i in seq_along(spalten_liste)) 
  {
    spalten_name <- spalten_liste[i]
    spalte_als_symbol <- rlang::sym(spalten_name) 
    
    # Aufruf der Plot-Funktion
    plot_univariate_histogram(column_name = !!spalte_als_symbol)
  }
  
  #Sendet Log über Erfolg
  cat("\n Univariate Histogramme wurden erstellt \n")
  return(invisible(NULL))
}
