#WestModell.R
#' Schätzung des Referenzmodells (Westdeutschland)
#' 
#' Diese Funktion isoliert die westdeutschen Wahlkreise, schätzt eine Beta-Regression 
#' mit gemischten Effekten (Random Intercept für Bundesländer) und exportiert die 
#' statistischen Kennzahlen.
#'
#' @param variable_list Vektor mit den Namen der Prädiktoren (z.B. vars_model_1)
#' @param model_label Text-Label für die Beschriftung von Tabellen und Dateien
fit_west_model <- function(variable_list, model_label) {
  
  # 1. Datenvorbereitung: Filterung auf Westdeutschland (Referenzraum)
  # Wir isolieren die Beobachtungen, für die die Variable 'ost' den Wert 0 hat.
  data_west <- data %>% 
    dplyr::filter(ost == 0)
  
  # Entfernen der Geometrie-Daten (sf-Objekt), um die Rechengeschwindigkeit von glmmTMB 
  # zu erhöhen und Kompatibilitätsprobleme bei der Schätzung zu vermeiden.
  if (inherits(data_west, "sf")) {
    data_west <- sf::st_drop_geometry(data_west)
  }
  
  # 2. Dynamische Modellformulierung
  # Verbindet die Variablennamen aus der Liste zu einem String für die Formel.
  predictors_str <- paste(variable_list, collapse = " + ")
  
  # Erstellt das Formel-Objekt: afd_prop als abhängige Variable, 
  # (1 | state) definiert den Random Intercept für die Bundesländer.
  formula_str <- paste("afd_prop ~", predictors_str, "+ (1 | state)")
  
  # 3. Modellschätzung mittels glmmTMB
  # Wir nutzen eine Beta-Regression (link = 'logit'), da Wahlanteile zwischen 0 und 1 liegen.
  model_w <- glmmTMB::glmmTMB(
    as.formula(formula_str),
    family = glmmTMB::beta_family(link = "logit"),
    data = data_west
  )
  
  # 4. Statistische Aufbereitung der Ergebnisse (Tidying)
  
  # A) Fixe Effekte: Koeffizienten der Prädiktoren (Einflussstärke und p-Werte)
  stats_fixed <- broom.mixed::tidy(model_w, effects = "fixed", conf.int = TRUE) %>%
    dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
    dplyr::mutate(
      # Rundung basierend auf der globalen Einstellung 'NACHKOMMASTELLEN'
      across(where(is.numeric), \(x) round(x, NACHKOMMASTELLEN)),
      # Markierung signifikanter Variablen (p < 0.05) für die schnelle Interpretation
      sig = ifelse(p.value < 0.05, "*", "") 
    )
  
  # B) Zufällige Effekte: Varianz zwischen den Bundesländern
  stats_random <- broom.mixed::tidy(model_w, effects = "ran_pars", conf.int = FALSE) %>%
    # Berechnung der Standardabweichung aus der geschätzten Varianz (Quadratwurzel)
    dplyr::mutate(std.dev = sqrt(estimate)) %>%
    dplyr::select(group, term, estimate, std.dev) %>%
    dplyr::mutate(across(where(is.numeric), \(x) round(x, NACHKOMMASTELLEN)))
  
  # 5. Export-Logik: Steuerung über globale Flag 'IS_TABLE_OUTPUT_ENABLED'
  if (exists("IS_TABLE_OUTPUT_ENABLED") && IS_TABLE_OUTPUT_ENABLED) {
    
    # Ausgabe der Ergebnistabellen direkt in die R-Konsole zur Sofortkontrolle
    cat(paste0("\n--- Ergebnisse für: ", model_label, " ---\n"))
    print(knitr::kable(stats_fixed, caption = paste("Fixe Effekte:", model_label)))
    print(knitr::kable(stats_random, caption = paste("Zufällige Effekte:", model_label)))
    
    # Zusammenstellung des Excel-Exports mit dynamischem Dateinamen
    file_name <- paste0("Ergebnisse_West_", model_label, ".xlsx")
    export_full_path <- file.path(EXPORT_PFAD_TABELLEN, file_name)
    
    # Erstellung einer Liste für unterschiedliche Arbeitsblätter (Sheets)
    output_list <- list(
      "Fixed Effects" = stats_fixed,
      "Random Effects" = stats_random
    )
    
    # Speicherung der Ergebnisse als Excel-Datei
    openxlsx::write.xlsx(output_list, file = export_full_path, overwrite = TRUE)
    message(paste0(">>> Excel gespeichert: ", export_full_path))
    
  } else {
    # Rückmeldung, falls der automatische Export deaktiviert wurde
    message(paste0("--- Datenausgabe ", model_label, " übersprungen ---"))
  }
  
  message(paste0("--- Berechnung ", model_label, " abgeschlossen ---\n"))
  
  # 6. Rückgabe des Modell-Objekts für nachfolgende Diagnosen oder Vorhersagen
  return(model_w)
}