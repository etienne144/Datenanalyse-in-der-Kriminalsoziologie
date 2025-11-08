# LoadScripts.R
# ************************************************************
# ZWECK: Dieses Skript dient als Manifest (Ladeliste) und lädt alle
#        separat organisierten R-Funktionsdateien in die aktuelle R-Sitzung.
#        Es stellt sicher, dass alle notwendigen Funktionen vor dem Start
#        der Hauptanalyse (MAIN.R) verfügbar sind.
# ************************************************************

# Lädt die Funktion zur Paketverwaltung (Installieren und Laden) sowie die Liste der benötigten Pakete.
source("Scripts/LoadPackages.R")

# Lädt die Funktionen zur Berechnung der univariaten deskriptiven Statistik.
source("Scripts/DeskriptiveStatistik.R")

# Lädt die Funktionen zur Visualisierung der univariaten deskriptiven Statistik (z.B. Histogramme).
source("Scripts/DeskriptiveStatistikVisualisierung.R")

# Lädt die Funktion zur Vorbereitung des Datensatzes (z.B. Erstellung abgeleiteter Variablen wie 'bildung_hoch').
source("Scripts/DatensatzVorbereiten.R")

# Lädt die Funktionen zur Berechnung der bivariaten deskriptiven Statistik (Gruppenvergleiche).
source("Scripts/BivariateDeskriptiveStatistik.R")

# Erfolgsmeldung in der Konsole
cat("\nScripte geladen\n")