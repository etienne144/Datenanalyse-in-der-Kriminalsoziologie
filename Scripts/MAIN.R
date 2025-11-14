# MAIN.R
# ************************************************************
# ZWECK: Dieses Skript dient als zentrale Steuereinheit (Master-Skript)
#        für die gesamte Datenanalyse und Visualisierung.
#        Es lädt alle Funktionen und Daten, setzt globale Parameter
#        und führt die univariaten und bivariaten Analysen durch.
# ************************************************************

# 1. SETUP:----
# ------------------------------------------------------------
# Initialisierung der Arbeitsumgebung
# ------------------------------------------------------------

# 1.1. Scripte laden
# Führt das Skript aus, das alle notwendigen Hilfsfunktionen (z.B. für Statistik, Plotting, Paket-Laden)
# in die aktuelle R-Sitzung lädt.
source("Scripts/LoadScripts.R")

# 1.2. Pakete laden
# Ruft die Funktion auf, um alle benötigten R-Pakete (z.B. ggplot2, dplyr) zu installieren und zu laden.
load_packages(required_packages)

# 1.3. Datensatz laden
# Lädt den gespeicherten Datensatz und weist ihn der Arbeitsvariable 'data' zu.
load("Data/Geovisualisierung.RData")
data <- geovisualisierung

# 1.4. Datensatz vorbereiten
# Ruft die Funktion auf, um neue, abgeleitete Variablen zu erstellen (z.B. 'bildung_hoch')
# und den Datensatz für die Analyse vorzubereiten.
data <- datensatz_vorbereiten(df = data)

# ************************************************************

# 2. Deskriptive Statistik: Einstellungen (Settings)----
# ------------------------------------------------------------
# Globale Steuerung der Ausgabe und Visualisierung
# ------------------------------------------------------------

# Steuert, ob die Ergebnisse der Analysen als Tabellen in der Konsole gedruckt werden sollen (TRUE/FALSE)
IS_TABLE_OUTPUT_ENABLED <- TRUE 

# Steuert, ob Plots/Diagramme erstellt und in der Konsole/im Plot-Fenster angezeigt werden sollen (TRUE/FALSE)
IS_PLOT_OUTPUT_ENABLED <- TRUE  

# Faktor zur Skalierung der Klassenanzahl (Bins) in Histogrammen.
# Je höher der Wert, desto feiner die Darstellung (mehr Säulen).
HISTOGRAM_BINS_FAKTOR <- 5 

EXPORT_PFAD_TABELLEN = "Tabellen"
EXPORT_PFAD_ABBILDUNGEN = "Abbildungen"

NACHKOMMASTELLEN <- 2
# 2.1.1 UNIVARIATE Analyse
# Hier könnten die Aufrufe für die univariate Analyse folgen, falls sie nicht auskommentiert sind:
spalten_von_interesse <- c("afd_prop", "linke_prop", "spd_prop", "b90_prop", "fdp_prop","union_prop")
all_stats_df <- run_descriptive_analysis(spalten_liste = spalten_von_interesse)
run_univariate_plotting(spalten_liste = spalten_von_interesse)

# 2.2.2 BIVARIATE Analyse: Definition aller Analysen
# ------------------------------------------------------------
# Definition der bivariaten Analysen, die durchgeführt werden sollen.
# Jede benannte Liste ('Analyse_...') enthält die Gruppierungsvariable und die Zielvariablen.
# ------------------------------------------------------------

  # Die zentrale Liste, die alle Bivariaten Analysen definiert
  bivariate_analysen_to_run <- list(
    
    # ERSTE ANALYSE: Vergleich der Bildungsproportionen nach Ost/West-Zugehörigkeit
      des_Bildungstand = list(
      grouping_variable = "ost", # Gruppierungsvariable
      target_variables = c("bildung_niedrig", "bildung_mittel", "bildung_hoch") # Zielvariablen
    ),
  
  # ZWEITE ANALYSE: Vergleich der Arbeitsquoten nach Ost/West-Zugehörigkeit
  der_Arbeitslosenquote = list(
    grouping_variable = "ost", # Gruppierungsvariable
    target_variables = c("arblQuote_gesamt", "arblQuote_maenner", "arblQuote_frauen", "arblQuote_jugend", "arblQuote_senioren") # Zielvariablen
  ),
  
  der_Wählerstimmen = list(
    grouping_variable = "ost", # Gruppierungsvariable
    target_variables = c("afd_prop", "linke_prop", "spd_prop", "b90_prop", "fdp_prop","union_prop")
  ),
  der_Empfängerinnen_von_Sozialleistungen
  = list(
    grouping_variable = "ost", # Gruppierungsvariable
    target_variables = c( "sgb2_nichterw")
  )
  
)

# Startet alle Bivariaten Analysen in einem Aufruf
# Ruft die Wrapper-Funktion auf, die nacheinander jede Analyse in der Liste durchführt.
# Die Ergebnisse werden in 'all_bivariate_results' gespeichert (eine Liste von Data Frames).
all_bivariate_results <- run_multiple_bivariate_analysis(
  analysen_liste = bivariate_analysen_to_run
)

# Startet alle Bivariaten Visualisierungen (Boxplots)
run_multiple_bivariate_plotting(
  analysen_liste = bivariate_analysen_to_run
)