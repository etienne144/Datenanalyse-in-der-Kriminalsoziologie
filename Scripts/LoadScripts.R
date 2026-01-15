# LoadScripts.R
# ************************************************************
# ZWECK: Dieses Skript dient als Ladeliste und lädt alle
#        separat organisierten R-Funktionsdateien in die aktuelle R-Sitzung.
#        Es stellt sicher, dass alle notwendigen Funktionen vor dem eigentlichen
#        Start der Hauptanalyse (MAIN.R) verfügbar sind.
# ************************************************************

# Lädt die Funktion zur Paketverwaltung (Installieren und Laden) sowie die Liste der benötigten Pakete.
source("Scripts/LoadPackages.R")

# Lädt die Funktionen zur Berechnung der univariaten deskriptiven Statistik.
source("Scripts/DeskriptiveStatistik.R")

# Lädt die Funktionen zur Visualisierung der univariaten deskriptiven Statistik (Histogramme).
source("Scripts/DeskriptiveStatistikVisualisierung.R")

# Lädt die Funktion zur Vorbereitung des Datensatzes (z.B. Erstellung abgeleiteter Variablen wie 'bildung_hoch').
source("Scripts/DatensatzVorbereiten.R")

# Lädt die Funktionen zur Berechnung der bivariaten deskriptiven Statistik (Gruppenvergleiche).
source("Scripts/BivariateDeskriptiveStatistik.R")

# Lädt die Funktionen zur Visualisierung der bivariaten deskriptiven Statistik (Boxplots).
source("Scripts/BivariateDeskriptiveStatistikVisualisierung.R")

# Lädt die Funktionen zur Visualisierung der bivariaten deskriptiven Statistik (Boxplots).
source("Scripts/VariablenMapping.R")

#Lädt die Funktion für die Einfche-Bet-Regression
source("Scripts/Einfach-Beta-Regression.R")

#Lädt die Funktion für die Korrelationsmatrix
source("Scripts/Korrelationsmatrix.R")

#Lädt die Funktion für den Test der Modellextrapolation
source("Scripts/ModellextrapolationCheck.R")

#Lädt die Funktion für das Westmodel
source("Scripts/WestModell.R")

#Lädt die Funktionen für die visualiserung
source("Scripts/WestModellVisualisierung.R")

# Erfolgsmeldung in der Konsole
cat("\nScripte geladen\n")