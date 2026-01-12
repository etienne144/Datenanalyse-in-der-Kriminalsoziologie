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

# 1.3. Datensatz laden und vorbereiten
# Ruft die Funktion auf, um neue, abgeleitete Variablen zu erstellen (z.B. 'bildung_hoch')
# und den Datensatz für die Analyse vorzubereiten.
data <- datensatz_vorbereiten()

# ************************************************************

# 2. Initial Data Analysis: ----
# ------------------------------------------------------------
# Globale Steuerung der Ausgabe und Visualisierung
# ------------------------------------------------------------

# Sollen Tbellen neu berechnet und überspeichert werden?
IS_TABLE_OUTPUT_ENABLED <- TRUE #(TRUE/FALSE)

# Sollen Abbildungen neu berechnet und überspeichert werden?
IS_PLOT_OUTPUT_ENABLED <- FALSE #(TRUE/FALSE)

# Faktor zur Skalierung der Klassenanzahl (Bins) in Histogrammen.
# Je höher der Wert, desto feiner die Darstellung (mehr Säulen).
# Nachkommastellen benutzten wir nur für den Tabellenoutput, NICHT für die Berechnungen
HISTOGRAM_BINS_FAKTOR <- 5
EXPORT_PFAD_TABELLEN = "Tabellen"
EXPORT_PFAD_ABBILDUNGEN = "Abbildungen"
NACHKOMMASTELLEN <- 2

# 2.1 UNIVARIATE Analyse
# ------------------------------------------------------------
# Hier könnten die Aufrufe für die univariate Analyse folgen
# Als Output gibt es einer Mastertabelle und für jede Variable eine eigene Tabelle
# ------------------------------------------------------------
spalten_von_interesse <- c("afd_prop", "linke_prop", "spd_prop", "b90_prop", "fdp_prop","union_prop")
all_stats_df <- run_descriptive_analysis(spalten_liste = spalten_von_interesse)
run_univariate_plotting(spalten_liste = spalten_von_interesse)

# 2.2 BIVARIATE Analyse: Definition aller Analysen
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

# 2.3 KORRELATIONSMATRIX
# ------------------------------------------------------------
# Hier erstelle wir für jedes Model unsere Korreltionsmatrix
# An diesen können wir prüfen, ob es mögliche Korrelationen gibt
# ------------------------------------------------------------
# 2.3 KORRELATIONSMATRIX
# ------------------------------------------------------------
# 2.3.1 Für ALLE Variablen
vars_all <- c(
  "ausl_proz", "gebursaldo", "wandssaldo", "migration","eigQuote", "wohnbestand", "einkommen", "bip_je_einwohner", "bildung_niedrig", "bildung_mittel", "bildung_hoch", "sgb2_empf", "sgb2_nichterw", "sgb2_auslaender_prozent","arblQuote_gesamt", "arblQuote_maenner", "arblQuote_frauen", "arblQuote_jugend", "arblQuote_senioren", "kath", "evang", "alter_bis_24", "alter_25_59")
plot_region_correlation(vars_all, "west", "Alle Variablen")
# ------------------------------------------------------------

# 2.3.2 Für unsere Variablen
vars_unsere <- c(
  "ausl_proz", "gebursaldo", "wandssaldo","eigQuote", "wohnbestand", "einkommen","bildung_hoch", "sgb2_auslaender_prozent","kath", "evang", "ost")
plot_region_correlation(vars_unsere, "west", "Unsere Variablen")
# ------------------------------------------------------------
#...Modell Lena, Etienn, Lars


# 2.4 Modellextrapolation Check
# ------------------------------------------------------------
# Hier prüfen wir, mit welchen Variablen wir gefahr laufen Vorhersagen
# in einen Bereich zu treffen, den unsere Trainingsdaten nicht hergeben
# ------------------------------------------------------------
check_within_interval(vars_unsere, "Unsere Variablen")


# ************************************************************

# 3. Beta-Regression----
# ------------------------------------------------------------
data <- datensatz_vorbereiten_regression(df = data)
# Die zentrale Liste, welche die Betaregressionsmodelle speichert
beta_analysen_to_run <- list(
  
  AfD_Einkommen = list(
    y_name = "afd_prop", # Abhängige Variable
    x_names = c("einkommen") # Unabhängige Variable(n)
  ),
   
  AFD_verlorenerOrt = list(
    y_name = "afd_prop", # Abhängige Variable                                   Hier falls ihr interaktionen prüfen wollt
    x_names = c( "gebursaldo", "wohnbestand", "wandssaldo", "arblQuote_jugend", "arblQuote_jugend:wohnbestand") # Unabhängige Variable(n)
  )
)

# Aufruf der Loop-Funktion
Einfach_Beta_Regressions_Loop(beta_analysen_to_run)
