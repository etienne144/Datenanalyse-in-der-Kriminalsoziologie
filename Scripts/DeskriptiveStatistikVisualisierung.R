# DeskriptiveStatistikVisualisierung.R

# ************************************************************
# 1. Kernfunktion: Erstellung eines univariaten Histogramms
# ************************************************************
#' Erstellt ein Histogramm zur Visualisierung der Verteilung einer einzelnen metrischen Variable.
#'
#' Die Anzahl der Bins wird mittels Sturges' Regel bestimmt
#' und durch den Faktor HISTOGRAM_BINS_FAKTOR skaliert.
#' Die Funktion verwendet die globale Variable 'data' und 'ggplot2'.
#'
#' @param column_name Die metrische Spalte, deren Verteilung dargestellt werden soll.
#' @return Der erzeugte Plot wird direkt in der Konsole ausgegeben; die Funktion gibt unsichtbar NULL zurück.
#' @export
plot_univariate_histogram <- function(column_name)
{
  # 1. Spaltennamen als Klartext-String für die Achsen-Beschriftung und den Titel erfassen
  column_name_str <- rlang::as_label(rlang::enquo(column_name))
  
  # 2. Den Wertebereich ermitteln, um die optimale Anzahl der Klassen (Bins) zu bestimmen (Sturges' Regel)
  daten_vektor <- data |> dplyr::pull({{ column_name }})
  anzahl_bins <- nclass.Sturges(daten_vektor)
  
  # 3. Das Histogramm mit ggplot2 erstellen
  histogram_plot <- ggplot2::ggplot(data,
                                    ggplot2::aes(x = {{ column_name }})) +
    
    # Zeichnen der Histogramm-Balken: Die Anzahl der Bins wird angepasst (skaliert mit FAKTOR)
    ggplot2::geom_histogram(bins = anzahl_bins * HISTOGRAM_BINS_FAKTOR,
                            fill = "skyblue",
                            color = "black",
                            alpha = 0.7) +
    
    # Beschriftungen und Titel setzen
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
#' Führt die Visualisierung aller univariat definierten Spalten durch.
#'
#' Iteriert über eine Liste von Spaltennamen und ruft 'plot_univariate_histogram()'
#' für jede Variable auf. Die Ausführung wird durch die globale Variable
#' 'IS_PLOT_OUTPUT_ENABLED' gesteuert.
#'
#' @param spalten_liste Vektor von Strings: Die metrischen Spalten, die visualisiert werden sollen.
#' @return Unsichtbar NULL. Gibt eine Meldung aus, wenn die Visualisierung übersprungen wurde.
#' @export
run_univariate_plotting <- function(spalten_liste)
{
  # Überprüfung: Führt die Visualisierung nur aus, wenn die Steuerungsvariable TRUE ist
  if (!(exists("IS_PLOT_OUTPUT_ENABLED") && IS_PLOT_OUTPUT_ENABLED)) {
    cat("IS_PLOT_OUTPUT_ENABLED ist FALSE. Univariate Visualisierung wurde uebersprungen.")
    return(invisible(NULL))
  }
  
  # Iteration: Geht nacheinander alle Spaltennamen durch
  for (i in seq_along(spalten_liste)) 
  {
    spalten_name <- spalten_liste[i]
    # Konvertierung des Strings in ein R-Objekt (Symbol) für die Übergabe
    spalte_als_symbol <- rlang::sym(spalten_name) 
    
    # Aufruf der Plot-Funktion für die aktuelle Variable
    plot_univariate_histogram(column_name = !!spalte_als_symbol)
  }
  
  # Sendet Log über Erfolg
  cat("\nUnivariate Histogramme wurden erstellt.\n")
  return(invisible(NULL))
}