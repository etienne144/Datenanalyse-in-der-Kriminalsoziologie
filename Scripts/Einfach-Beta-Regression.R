#Einfach-Beta-Regression

#' Führt eine Beta-Regression durch und gibt das glmmTMB-Modellobjekt zurück.
#'
#' @param y_name String: Name der abhängigen Variable (Muss Anteilswert zwischen 0 und 1 sein).
#' @param x_names Vektor von Strings: Namen der unabhängigen Variablen (inkl. Kontrollvariablen).
#' @return Das glmmTMB-Modellobjekt.
Einfach_Beta_Regression <- function(y_name, x_names)
{
  # Formel dynamisch erstellen: y ~ x1 + x2 + ...
  formula_str <- paste(y_name, "~", paste(x_names, collapse = " + "))
  
  # 1. Regressionsmodell erstellen
  model <- glmmTMB::glmmTMB(
    formula = as.formula(formula_str),
    family = glmmTMB::beta_family(link = "logit"),
    data = data # Nutzung der globalen Variable 'data'
  )
  
  # Der Plot und der Summary-Export werden in der Loop-Funktion gehandhabt.
  return(model)
}
#' Erstellt und exportiert einen Visualisierungs-Plot für das Beta-Regressionsmodell.
#'
#' @param model Das glmmTMB-Modellobjekt.
#' @param y_name String: Name der abhängigen Variable.
#' @param x_name String: Name der unabhängigen Variable, die visualisiert werden soll (muss die erste UV sein).
#' @param analyse_name String: Name der Analyse (für den Dateinamen).
#' @param output_dir String: Der Ordner, in den das Bild exportiert wird.
plot_beta_regression <- function(model, y_name, x_name, analyse_name)
{
  # 1. Sicherstellen, dass der Output-Ordner existiert
  dir.create(EXPORT_PFAD_ABBILDUNGEN, recursive = TRUE, showWarnings = FALSE)
  
  # 2. Daten für die Vorhersagelinie erstellen
  # Da die Visualisierung kompliziert ist, wenn es Kontrollvariablen gibt, 
  # erstellen wir hier nur die Vorhersage für die x_name-Variable, 
  # wobei andere Kovariaten (falls vorhanden) auf ihren Mittelwert gesetzt werden müssten.
  # Für eine einfache Darstellung verwenden wir die Originaldaten, um die Linie zu visualisieren:
  
  # Vorhersagewerte auf den Originaldaten erzeugen
  data$pred_y <- predict(model, type = "response") 
  
  # 3. Plot-Titel und Achsenbeschriftung erstellen
  plot_titel <- paste0("Beta-Regression: Beziehung zwischen ", x_name, " und ", y_name)
  
  # 4. Plot erstellen
  plot_obj <- ggplot2::ggplot(data, 
                              ggplot2::aes(x = !!rlang::sym(x_name), 
                                           y = !!rlang::sym(y_name))) +
    
    # A. Originaldatenpunkte
    ggplot2::geom_point(alpha = 0.4, color = "gray50", 
                        # Ästhetik auf Original-Y-Variable beziehen
                        ggplot2::aes(y = !!rlang::sym(y_name))) + 
    
    # B. Vorhersagelinie (Glättung der Vorhersagewerte)
    # Wir plotten die Vorhersagewerte gegen die UV.
    ggplot2::geom_line(ggplot2::aes(y = pred_y), 
                       color = "steelblue", 
                       linewidth = 1.2) + 
    
    # 5. Beschriftungen
    ggplot2::labs(x = x_name,
                  y = paste0("Vorhergesagter Anteil (", y_name, ")"),
                  title = plot_titel) +
    ggplot2::theme_minimal()
  
  # 6. Export als PNG
  file_path <- file.path(EXPORT_PFAD_ABBILDUNGEN, paste0("BetaReg_Plot_", analyse_name, ".png"))
  
  ggplot2::ggsave(
    filename = file_path,
    plot = plot_obj,
    width = 18, height = 12, units = "cm"
  )
  cat(paste0("  -> Plot exportiert nach: ", file_path, "\n"))
  
  # Wichtig: Die temporäre Spalte wieder entfernen!
  data$pred_y <- NULL 
}

#' Führt mehrere Beta-Regressionen aus und exportiert Summarys sowie Plots.
#'
#' @param analysen_liste Eine benannte Liste, die y_name und x_names für jede Analyse definiert.
#' @return Unsichtbar NULL. Die Funktion speichert Dateien auf der Festplatte.
Einfach_Beta_Regressions_Loop <- function(analysen_liste)
{
  dir.create(EXPORT_PFAD_TABELLEN, recursive = TRUE, showWarnings = FALSE)
  dir.create(EXPORT_PFAD_ABBILDUNGEN, recursive = TRUE, showWarnings = FALSE)
  
  for (analyse_name in names(analysen_liste)) {
    
    aktuelle_analyse <- analysen_liste[[analyse_name]]
    y_name <- aktuelle_analyse$y_name
    x_names <- aktuelle_analyse$x_names
    
    # 1. Modell erstellen
    model <- Einfach_Beta_Regression(y_name = y_name, x_names = x_names)
    
    # 2. Summary erstellen und exportieren (Nutzt den Analysennamen für die Datei)
    summary_name <- paste0(EXPORT_PFAD_TABELLEN, "/BetaReg_", analyse_name, ".docx")
    
    modelsummary::modelsummary(
      model,
      shape = term ~ component,
      statistic = "conf.int",
      output = summary_name
    )
    
    # 3. Abbildung erstellen und exportieren (bezieht sich auf die erste unabhängige Variable)
    unabhaengige_variable_fuer_plot <- x_names[1] 
    plot_beta_regression(
      model = model,
      y_name = y_name,
      x_name = unabhaengige_variable_fuer_plot,
      analyse_name = analyse_name # Für den Dateinamen
    )
      
  }
  
  cat("\nAlle Beta-Regressionen abgeschlossen.\n")
  invisible(NULL)
}