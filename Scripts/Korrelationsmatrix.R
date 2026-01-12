# Korrelationsmatrix

plot_region_correlation <- function(variables, region = "west", model_bezeichnung) {
  
  # 0. Early Exit
  if (!IS_PLOT_OUTPUT_ENABLED) {
    message("Erstellung der Korrelationsmatrix wird übersprungen")
    return(invisible(NULL))
  }
  
  # 1. Filter-Logik
  # Nutzt das globale Objekt 'data'
  region_low <- tolower(region)
  
  if (region_low == "west") {
    df_filtered <- data %>% filter(ost == 0)
    titel_zusatz <- "Westdeutschland"
  } else if (region_low == "ost") {
    df_filtered <- data %>% filter(ost == 1)
    titel_zusatz <- "Ostdeutschland"
  } else {
    stop("Region muss 'west' oder 'ost' sein.")
  }
  
  # 2. Daten vorbereiten
  # 'ost' ausschließen und prüfen, ob Variablen im df existieren
  vars_to_calc <- setdiff(variables, "ost")
  vars_to_calc <- intersect(vars_to_calc, colnames(data))
  
  cor_data <- df_filtered %>%
    sf::st_drop_geometry() %>%
    dplyr::select(all_of(vars_to_calc))
  
  # 3. Korrelation berechnen (Spearman)
  cor_matrix <- cor(cor_data, method = "spearman", use = "complete.obs")
  
  # 4. Plot erstellen
  plot <- ggcorrplot(cor_matrix, 
                     type = "upper", 
                     lab = TRUE, 
                     lab_size = 3,
                     title = paste("Korrelationsmatrix:", model_bezeichnung, "-", titel_zusatz),
                     colors = c("#E46726", "white", "#6D9EC1"))
  
  # 5. Speichern
  # Pfad erstellen (Region im Dateinamen verhindert Überschreiben)
  file_name <- paste0("Korrelation_", model_bezeichnung, "_", region_low, ".png")
  file_path <- file.path(EXPORT_PFAD_ABBILDUNGEN, file_name)
  
  ggplot2::ggsave(
    filename = file_path,
    plot = plot,
    device = "png",
    width = 18,
    height = 12,
    units = "cm",
    dpi = 300
  )
  cat(paste0("\n--- Korrelationsmatrix ", model_bezeichnung, " (", region_low, ") erstellt ---\n"))
  
  return(invisible(NULL))
}