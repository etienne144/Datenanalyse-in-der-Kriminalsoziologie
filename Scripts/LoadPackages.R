# LoadPackages.R

# Liste der benötigten Pakete
required_packages <- c("here",    
                       "tidyverse",
                       "ggplot2"#,
                       #"see"
                       )

load_packages <- function(packages) 
{
  # 1. Die Iteration starten
  invisible(lapply(packages, function(pkg) {
    
    # 2. Prüfen, ob das Paket installiert ist
    if (!requireNamespace(pkg, quietly = TRUE)) {
      
      # 3. Das Paket installieren (falls es fehlt)
      install.packages(pkg, dependencies = TRUE)
    }
    
    # 4. Das Paket laden
    library(pkg, character.only = TRUE)
  }))
}

# Führe die Funktion aus
load_packages(required_packages)

#Load die anderen Scripts
source("Scripts/DeskriptiveStatistik.R")

# Gib eine kurze Meldung aus, dass die Pakete geladen wurden
print("Alle benötigten Pakete wurden geladen.")