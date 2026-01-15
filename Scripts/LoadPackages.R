# LoadPackages.R
# ************************************************************
# ZWECK: Dieses Skript definiert die Liste der benötigten R-Pakete
#        und stellt eine Funktion bereit, um diese zu prüfen,
#        gegebenenfalls zu installieren und anschließend zu laden.
# ************************************************************

# Liste der benötigten Pakete
# Dieser Vektor enthält die Namen aller Pakete, die für die Ausführung der Analyse
# (Datenmanipulation, Statistik, Visualisierung) zwingend erforderlich sind.
required_packages <- c("here",         # Für die einfache Pfadverwaltung in Projekten
                       "tidyverse",    # Kernpaket für Datenmanipulation (enthält dplyr, ggplot2, etc.)
                       "ggplot2",      # Separate Nennung, falls tidyverse nicht geladen wird (redundant, aber klar)
                       "ggcorrplot",
                       "ggalluvial",  # Spezielles Paket für Alluvial-Diagramme (Visualisierung von Flussdaten/Kategorien)
                       "openxlsx",    # zum exportieren als exceldatei
                       "glmmTMB",       #für die Beta-Regression
                       "pandoc",       #auch für Beta
                       "broom",
                       "kableExtra",
                       "knitr",
                       "sf", #braucht es für die Geodaten
                       "pradadata", #der Datensatz
                       "DHARMa", #für die simulation der Residuen
                       "broom.mixed" #keine Ahnung was der Unterschied zu nur broom ist
                       )

# ************************************************************
# Funktion zum Prüfen, Installieren und Laden von Paketen
# ************************************************************
#' Prüft eine Liste von Paketen, installiert fehlende und lädt alle für die Sitzung.
#'
#' Diese Funktion ist robust und stellt sicher, dass die Umgebung korrekt initialisiert wird.
#' Sie ist optimiert, um keine unnötigen Meldungen in der Konsole zu erzeugen (invisible(lapply)).
#'
#' @param packages Vektor von Strings: Die Namen der zu prüfenden und zu ladenden Pakete.
#' @return Unsichtbar NULL. Gibt eine Erfolgsmeldung in die Konsole aus.
#' @export
load_packages <- function(packages) 
{
  # 1. Die Iteration starten: Geht alle Pakete in der Liste nacheinander durch.
  invisible(lapply(packages, function(pkg) {
    
    # 2. Prüfen, ob das Paket bereits in der Bibliothek vorhanden ist
    # 'requireNamespace' ist eine saubere Methode zur Prüfung der Installation.
    if (!requireNamespace(pkg, quietly = TRUE)) {
      
      # 3. Das Paket installieren (falls es fehlt)
      # install.packages() wird nur ausgeführt, wenn das Paket nicht gefunden wurde.
      install.packages(pkg, dependencies = TRUE)
    }
    
    # 4. Das Paket laden: Macht die Funktionen des Pakets in der Sitzung verfügbar.
    # 'character.only = TRUE' ist notwendig, da 'pkg' ein String ist.
    library(pkg, character.only = TRUE)
  }))
  cat("\nPackages geladen\n")
}