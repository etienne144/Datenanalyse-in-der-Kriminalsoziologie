# BivariateDeskriptiveStatistikVisualisierung.R

# ************************************************************
# 1. Kernfunktion: Erstellt den Multi-Boxplot
# ************************************************************
#' Erstellt eine einzige Boxplot-Abbildung für alle Zielvariablen (target_variables), 
#' aufgeschlüsselt nach der Gruppierungsvariable (grouping_variable).
#'
#' @param df Data Frame: Der zu visualisierende Datensatz.
#' @param target_variables Vektor von Strings: Die metrischen Spalten, die in einem Plot zusammengefasst werden.
#' @param grouping_variable String: Die kategoriale Spalte zur Gruppierung.
#' @return Der erzeugte Plot wird direkt ausgegeben; die Funktion gibt unsichtbar NULL zurück.
plot_multiple_boxplots <- function(df, target_variables, grouping_variable, analyse_name_str)
{
  # 1. Daten umstrukturieren (Pivot: Von "breit" nach "lang")
  # Alle Zielvariablen werden in zwei neue Spalten überführt (Variable und Wert)
  df_long <- df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(target_variables), # Die Spalten, die umgeformt werden sollen
      names_to = "Zielvariable",             # Name der neuen Spalte für die Variablennamen
      values_to = "Wert"                     # Name der neuen Spalte für die Werte
    ) |>
    # Stellt sicher, dass die Gruppierungsvariable als Faktor behandelt wird
    dplyr::mutate(
      Gruppe = factor(!!rlang::sym(grouping_variable)),
      
      #Definiert Zielvariable als Faktor mit Levels in der Reihenfolge von target_variables
      Zielvariable = factor(
        Zielvariable, 
        levels = target_variables, 
        ordered = TRUE
      )
    )
  
  # 2. Beschriftungen und Klartext-Lookup
  lesbarer_analyse_name <- gsub("_", " ", analyse_name_str) #ersetzt die "_" durch Leerzeichen
  x_achsen_str <- sub("^[^_]*_", "", analyse_name_str) # "des_Bildungsstand" -> "Bildungsstand"
  fill_label <- GROUP_KLARNAMEN[grouping_variable] %||% grouping_variable #Legenden-Titel (z.B. "Ost-/Westdeutschland")
  erste_zielvariable <- target_variables[1]# C. Y-Achsen-Einheit (Nimmt die Einheit der ersten Target-Variable als repräsentativ)
  y_label <- EINHEITEN_MAPPING[erste_zielvariable] %||% "Wert" # Fallback auf "Wert"
  y_label <- ifelse(is.na(y_label), "In Prozent", y_label)
  x_labels_map <- KLARNAMEN[target_variables]# Nutzt KLARNAMEN, um z.B. aus 'afd_prop' -> 'AfD' zu machen.
  legenden_werte_map <- GRUPPEN_WERTE_KLARTEXT[[grouping_variable]]# E. Legenden-Werte (z.B. 0 -> Westdeutschland)
  title_str <- paste0("Verteilung ",lesbarer_analyse_name,", gruppiert nach ", fill_label)
  
  # 3. Der Boxplot mit ggplot2
  boxplot_plot <- ggplot2::ggplot(df_long, 
                                  ggplot2::aes(x = Zielvariable, 
                                               y = Wert, 
                                               fill = Gruppe)) + # Füllung basiert auf der Gruppe
    
    # Position_dodge trennt die Boxplots der Gruppen für jede Zielvariable
    ggplot2::geom_boxplot(alpha = 0.7, 
                          position = ggplot2::position_dodge(width = 0.8),
                          outlier.shape = 1) + 
    
    ggplot2::labs(
      x = x_achsen_str,
      y = y_label,
      title = title_str,
      fill = grouping_variable # Legende
    ) +
    # SKALIERUNG: Nutzt die Klartext-Mappings für X-Achse und Legenden
    ggplot2::scale_x_discrete( 
      labels = x_labels_map # Labels für jede Box (z.B. AfD, Die Linke)
    ) +
    ggplot2::scale_fill_manual(
      name = "Legende:",
      values = c("0" = "#F8766D", "1" = "#00BFC4"), 
      labels = legenden_werte_map # Dynamische Labels (z.B. Westdeutschland, Ostdeutschland)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) # X-Achsen-Text drehen
  
  print(boxplot_plot)
  
  return(boxplot_plot)
}

# ************************************************************
# 2. Wrapper-Funktion für die Abarbeitung der gesamten Analyse-Liste (Unverändert)
# ************************************************************
#' Wrapper-Funktion, die alle in der analysen_liste definierten bivariaten Visualisierungen erstellt.
#' Die Funktion ruft 'plot_multiple_boxplots' für jedes Set von Zielvariablen auf.
#'
#' @param analysen_liste Eine Liste von Listen, die jede Analyse definiert.
#' @return Unsichtbar NULL.
#' @export
run_multiple_bivariate_plotting <- function(analysen_liste)
{
  # Prüft die globale Einstellung
  if (!(exists("IS_PLOT_OUTPUT_ENABLED") && IS_PLOT_OUTPUT_ENABLED)) {
    return(invisible(NULL))
  }
  
  # 1. Iteriere über jede definierte Analyse
  for (analyse_name in names(analysen_liste)) {
    
    aktuelle_analyse <- analysen_liste[[analyse_name]]
    
    # 2. Extrahiere die Parameter
    grouping_var <- aktuelle_analyse$grouping_variable
    target_vars <- aktuelle_analyse$target_variables
    
    cat(paste0("\n--- Starte Boxplot-Visualisierung: ", analyse_name, " ---\n"))
    
    # 3. Führe die Kern-Plot-Funktion aus (jetzt plot_multiple_boxplots!)
    current_plot <- plot_multiple_boxplots(
      df = data, # Übergeben Sie den Datensatz explizit
      grouping_variable = grouping_var,
      target_variables = target_vars,
      analyse_name_str = analyse_name
    )
    file_path <- file.path(EXPORT_PFAD_ABBILDUNGEN, paste0("Boxplots_", analyse_name, ".png"))
    
    ggplot2::ggsave(
      filename = file_path,
      plot = current_plot,
      device = "png",         # Dateiformat
      width = 18,             # Breite (in cm)
      height = 12,            # Höhe (in cm)
      units = "cm"            # Einheiten
    )
  }
  
  cat("\nBivariaten Visualisierungen abgeschlossen.\n")
  return(invisible(NULL))
}