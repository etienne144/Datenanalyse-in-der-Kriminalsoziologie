#ModellextrapolationCheck

# Basierend auf dem Beispiel von Nold, erweitert um Excel-Export und Output-Check
check_within_interval <- function(target_vars, modell_name) {
  
  region_var <- "ost"
  region_values <- c(1, 0)
  tolerance <- 0.20
  
  # 0. Early Exit
  if (!IS_TABLE_OUTPUT_ENABLED) {
    message(paste0("Modellextrapolationscheck für ", modell_name, " übersprungen"))
    return(invisible(NULL))
  }
  
  # 1. Geometrie rausnehmen & Daten vorbereiten
  df_clean <- data
  if (inherits(data, "sf")) {
    df_clean <- sf::st_drop_geometry(data)
  }
  
  # Nur die gewünschten Variablen nehmen, die auch im Datensatz existieren
  numeric_vars <- intersect(target_vars, colnames(df_clean))
  
  diagnose_results <- data.frame(Variable = numeric_vars, stringsAsFactors = FALSE)
  diagnose_results$Check <- "Modellextrapolation"
  
  # 2. Berechnungsschleife
  for (i in seq_along(numeric_vars)) {
    var <- numeric_vars[i]
    var_sym <- rlang::sym(var)
    reg_sym <- rlang::sym(region_var)
    
    stats <- df_clean %>%
      dplyr::filter(!!reg_sym %in% region_values) %>%
      dplyr::group_by(!!reg_sym) %>%
      dplyr::summarise(
        actual_mean = mean(!!var_sym, na.rm = TRUE),
        min_val = min(!!var_sym, na.rm = TRUE),
        max_val = max(!!var_sym, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        midpoint = (min_val + max_val) / 2,
        lower = midpoint * (1 - tolerance),
        upper = midpoint * (1 + tolerance),
        within = actual_mean >= lower & actual_mean <= upper
      )
    
    # Wenn der Check für beide Regionen (Ost & West) bestanden wurde
    if (nrow(stats) == 2 && all(stats$within)) {
      diagnose_results$Check[i] <- "Keine Modellextrapolation"
    }
  }
  
  # 3. Konsolenausgabe
  cat(paste0("\n--- Ergebnisse Modellextrapolationscheck: ", modell_name, " ---\n"))
  print(diagnose_results)
  cat("------------------------------------------------------------\n")
  
  # 4. Export mit dynamischem Dateinamen und Sheetnamen
  file_name <- paste0("Modellextrapolationscheck.xlsx")
  export_pfad <- file.path(EXPORT_PFAD_TABELLEN, file_name)
  
  # Wir begrenzen den Sheetnamen auf 31 Zeichen (Excel-Limit)
  sheet_name_clean <- substr(modell_name, 1, 31)
  
  openxlsx::write.xlsx(
    diagnose_results, 
    file = export_pfad, 
    overwrite = TRUE, 
    sheetName = sheet_name_clean
  )
  
  cat(paste0("Datei gespeichert unter: ", file_name,"/",sheet_name_clean, "\n"))
  
  return(invisible(diagnose_results))
}