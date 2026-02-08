# ******************************************************************************
# MAIN SCRIPT: Analyse der AfD-Wahlergebnisse
# ******************************************************************************
# ZWECK:
#   Dieses Skript fungiert als zentrale Steuereinheit ("Master-File").
#   Es führt keine Berechnungen direkt durch, sondern ruft spezialisierte 
#   Funktionen auf, um den gesamten Analyseprozess zu steuern:
#     1. Datenimport & Bereinigung
#     2. Deskriptive Statistik (Univariat & Bivariat)
#     3. Korrelationsanalysen
#     4. Regressionsmodelle (Beta-Regression)
#     5. Diagnostik & Residuenanalyse
#
# AUTOREN: [Lena Bayer, Lars Materne, Etienne Palitza]
# DATUM:   [Aktuelles Datum]
# ******************************************************************************

# ==============================================================================
# 1. SETUP & INITIALISIERUNG
# ==============================================================================
# Hier wird die R-Arbeitsumgebung vorbereitet.

# 1.1 Hilfsskripte laden
# Lädt unsere Bibliothek an selbstgeschriebenen Funktionen (für Plots, Statistik etc.)
source("Scripts/LoadScripts.R")

# 1.2 R-Pakete laden
# Installiert (falls nötig) und lädt externe Bibliotheken wie ggplot2, dplyr, glmmTMB.
load_packages(required_packages)

# 1.3 Datensatz laden
# Importiert die Rohdaten (Excel/Shapefiles), bereinigt sie und erstellt
# neue Variablen (z.B. Zusammenfassung von Bildungsständen).
# Das Ergebnis ist der Hauptdatensatz 'data'.
data <- datensatz_vorbereiten()

# ==============================================================================
# 2. GLOBALE EINSTELLUNGEN
# ==============================================================================
# Steuerung des Outputs: Sollen Dateien neu erstellt werden?

IS_TABLE_OUTPUT_ENABLED <- TRUE  # Bei TRUE werden Excel-Tabellen exportiert
IS_PLOT_OUTPUT_ENABLED  <- TRUE  # Bei TRUE werden Grafiken als PNG gespeichert

# Pfade für den Export
EXPORT_PFAD_TABELLEN    <- "Tabellen"
EXPORT_PFAD_ABBILDUNGEN <- "Abbildungen"

# Darstellungsparameter
HISTOGRAM_BINS_FAKTOR   <- 4  # Feinheit der Balken in Histogrammen
NACHKOMMASTELLEN        <- 2  # Rundung in Tabellen


# ==============================================================================
# 3. DESKRIPTIVE ANALYSE (Daten kennenlernen)
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Univariate Analyse (Verteilung einzelner Variablen)
# ------------------------------------------------------------------------------
# Wir betrachten zunächst nur die Wahlergebnisse der Parteien.
spalten_von_interesse <- c("afd_prop", "linke_prop", "spd_prop", 
                           "b90_prop", "fdp_prop", "ost")

# Berechnung der Kennzahlen (Mittelwert, Median, SD)
all_stats_df <- run_descriptive_analysis(spalten_liste = spalten_von_interesse)

# Erstellung von Histogrammen und Boxplots
run_univariate_plotting(spalten_liste = spalten_von_interesse)


# ------------------------------------------------------------------------------
# 3.2 Bivariate Analyse (Zusammenhänge zwischen zwei Variablen)
# ------------------------------------------------------------------------------
# Wir vergleichen verschiedene sozioökonomische Kennzahlen zwischen 
# West- und Ostdeutschland (Gruppierungsvariable: "ost").

bivariate_analysen_to_run <- list(
  
  # A) Bildungsstand im Vergleich
  des_Bildungstand = list(
    grouping_variable = "ost",
    target_variables  = c("bildung_niedrig", "bildung_mittel", "bildung_hoch")
  ),
  
  # B) Arbeitsmarktstatistiken
  der_Arbeitslosenquote = list(
    grouping_variable = "ost",
    target_variables  = c("arblQuote_gesamt", "arblQuote_maenner", 
                          "arblQuote_frauen", "arblQuote_jugend", 
                          "arblQuote_senioren")
  ),
  
  # C) Wahlergebnisse
  der_Wählerstimmen = list(
    grouping_variable = "ost",
    target_variables  = c("afd_prop", "linke_prop", "spd_prop", 
                          "b90_prop", "fdp_prop", "union_prop")
  ),
  
  # D) Sozialleistungen
  der_Empfängerinnen_von_Sozialleistungen = list(
    grouping_variable = "ost",
    target_variables  = c("sgb2_nichterw")
  )
)

# Durchführung der Berechnungen (T-Tests / Mittelwertvergleiche)
all_bivariate_results <- run_multiple_bivariate_analysis(
  analysen_liste = bivariate_analysen_to_run
)

# Visualisierung der Ergebnisse (Vergleichs-Boxplots)
run_multiple_bivariate_plotting(
  analysen_liste = bivariate_analysen_to_run
)


# ------------------------------------------------------------------------------
# 3.3 Dichtekurven (Density Plots)
# ------------------------------------------------------------------------------
# Detaillierter Vergleich der Verteilungen zwischen Ost und West für ausgewählte Variablen.

# Wirtschaft
vars_sozio <- c("einkommen", "bip_je_einwohner")
plot_multiple_density(data, vars_sozio, "ost", "Wirtschaftsstruktur")

# Demografie
vars_sozio <- c("gebursaldo", "wandssaldo")
plot_multiple_density(data, vars_sozio, "ost", "Bevölkerungsentwicklung")

# Religion
vars_sozio <- c("kath", "evang")
plot_multiple_density(data, vars_sozio, "ost", "Religion")


# ------------------------------------------------------------------------------
# 3.4 Korrelationsmatrizen
# ------------------------------------------------------------------------------
# Prüfung auf Multikollinearität (hängen die unabhängigen Variablen zu stark zusammen?)

# 1. Übersicht über ALLE potenziellen Variablen
vars_all <- c(
  "ausl_proz", "gebursaldo", "wandssaldo", "migration", "eigQuote", 
  "wohnbestand", "einkommen", "bip_je_einwohner", 
  "bildung_niedrig", "bildung_mittel", "bildung_hoch", 
  "sgb2_empf", "sgb2_nichterw", "sgb2_auslaender_prozent",
  "arblQuote_gesamt", "arblQuote_maenner", "arblQuote_frauen", 
  "arblQuote_jugend", "arblQuote_senioren", "kath", "evang", 
  "alter_bis_24", "alter_25_59"
)
plot_region_correlation(vars_all, "west", "Alle Variablen")

# 2. Fokus auf unsere ausgewählten Variablen
vars_unsere <- c(
  "ausl_proz", "gebursaldo", "wandssaldo", "eigQuote", "wohnbestand", 
  "einkommen", "bildung_hoch", "sgb2_auslaender_prozent", "kath", "evang"
)
plot_region_correlation(vars_unsere, "west", "Unsere Variablen")

# 3. Spezifische Korrelationen pro Modell-Idee
# Modell I (Kultur/Religion/Bildung)
vars_model_I <- c("bildung_hoch", "kath", "evang", "kath_sq", "evang_sq")
plot_region_correlation(vars_model_I, "west", "Variablen Modell_I")

# Modell II (Ökonomie/Demografie)
vars_model_II <- c("gebursaldo", "wandssaldo", "einkommen", "einkommen_sq")
plot_region_correlation(vars_model_II, "west", "Variablen Modell_II")

# Modell III (Konflikttheorie Wohnen)
vars_model_III <- c("wohnbestand", "wohnbestand_sq", "ausl_proz", "einkommen", "einkommen_sq")
plot_region_correlation(vars_model_III, "west", "Variablen Modell_III")

vars_model_gesamt <- c("bildung_niedrig", "kath", "evang", "kath_sq", "evang_sq","gebursaldo", "einkommen", "einkommen_sq","wohnbestand", "wohnbestand_sq")
plot_region_correlation(vars_model_gesamt, "west", "Variablen Modell_gesamt")
# ------------------------------------------------------------------------------
# 3.5 Extrapolations-Check (Support Overlap)
# ------------------------------------------------------------------------------
# Wir prüfen, ob die Ost-Daten im Wertebereich der West-Daten liegen.
# Falls nein, wäre eine Vorhersage für den Osten problematisch.
check_within_interval(vars_unsere, "Modell Gesamt-Check")


# ==============================================================================
# 4. REGRESSIONSANALYSE (Beta-Regression)
# ==============================================================================
# Ziel: Erklärung der AfD-Anteile basierend auf den West-Wahlkreisen.

# Vorbereitung: Transformation der Daten für die Regression
# (z.B. Skalierung von Variablen, Quadrieren von Termen, Logarithmieren)
data <- datensatz_vorbereiten_regression(data)


# ------------------------------------------------------------------------------
# 4.1 Modellierung & Diagnostik
# ------------------------------------------------------------------------------

# --- Modell I (Kultur & Bildung) ---
west_model_i <- fit_west_model(vars_model_I, model_label = "Modell_I")
visualize_west_diagnostics(west_model_i, "Modell_I")

# --- Modell II (Sozioökonomie) ---
west_model_ii <- fit_west_model(vars_model_II, model_label = "Modell_II")
visualize_west_diagnostics(west_model_ii, "Modell_II")

# --- Modell III (Wohnraum & Konflikt) ---
west_model_iii <- fit_west_model(vars_model_III, model_label = "Modell_III")
visualize_west_diagnostics(west_model_iii, "Modell_III")

# --- Gesamtmodell ---
west_model_gesamt <- fit_west_model(vars_model_gesamt, model_label = "Modell_gesamt")
visualize_west_diagnostics(west_model_gesamt, "Modell_gesamt")

# ------------------------------------------------------------------------------
# 4.2 Residuenanalyse Westdeutschland (Wo irrt das Modell?)
# ------------------------------------------------------------------------------
# Wir berechnen die Abweichung (Residuum) zwischen der Modell-Vorhersage
# und dem tatsächlichen Wahlergebnis.
# --- Analyse Modell I ---
calculate_and_export_residuals(west_model_i, "west", "Model_I")
plot_residual_histogram(west_model_i, "west", "Model_I")
plot_residual_map(west_model_i,  "west", "Modell_I")

# --- Analyse Modell II ---
calculate_and_export_residuals(west_model_ii, "west", "Model_II")
plot_residual_histogram(west_model_ii,"west", "Model_II")
plot_residual_map(west_model_ii,  "west", "Modell_II")

# --- Analyse Modell III ---
calculate_and_export_residuals(west_model_iii,"west", "Model_III")
plot_residual_histogram(west_model_iii, "west", "Model_III")
plot_residual_map(west_model_iii, "west", "Modell_III")

# --- Analyse Modell IV ---
calculate_and_export_residuals(west_model_gesamt,"west", "Model_gesamt")
plot_residual_histogram(west_model_gesamt, "west", "Model_gesamt")
plot_residual_map(west_model_gesamt, "west", "Modell_gesamt")



# ==============================================================================
# 5. TRANSFERANALYSE
# ==============================================================================
# ZIEL:
# Wir testen die Generalisierbarkeit der West-Modelle.
# Jede verbleibende Abweichung deutet auf spezifische "Ost-Effekte" (Kultur) hin.
# ------------------------------------------------------------------------------
# 5.1 Der Niveau-Check (Tabellarischer Beweis)
# ------------------------------------------------------------------------------

# A. Den Benchmark setzen (Das "Null-Szenario")
# Wir berechnen zuerst, wie hoch die AfD im Osten durchschnittlich ist,
# OHNE dass wir irgendwelche Erklärungsfaktoren berücksichtigen.
# Das ist unser Referenzwert (ca. 19,5%).
ost_null_model <- calculate_null_model()

# Wir speichern uns diesen "Basis-Wert" als Anker für alle Vergleiche
null_intercept <- fixef(ost_null_model)$cond["(Intercept)"]
check_ost_niveau(ost_null_model, "Nullmodell", null_intercept)


# B. Die Modelle testen (Struktur vs. Kultur)
# Wir wenden jetzt die Logik aus dem Westen (Koeffizienten) auf den Osten an.
# Die Funktion 'check_ost_niveau' berechnet dabei:
# 1. Die Prognose: Was "denkt" das Modell, wie stark die AfD sein müsste?
# 2. Den Fehler: Wie viele Prozentpunkte fehlen zur Realität?

# Test 1: Reicht Kultur & Bildung als Erklärung?
ost_model_i <- ost_transfer(west_model_i, "Modell_I")
check_ost_niveau(ost_model_i, "Modell_I", null_intercept)

# Test 2: Reicht Bevölkerungsentiwcklung?
ost_model_ii <- ost_transfer(west_model_ii, "Modell_II_Econ")
check_ost_niveau(ost_model_ii, "Modell_II_Econ", null_intercept)

# Test 3: Die Konflikttheorie
ost_model_iii <- ost_transfer(west_model_iii, "Modell_III_Konflikt")
check_ost_niveau(ost_model_iii, "Modell_III_Konflikt", null_intercept)

# Test 4: Das Gesamtmodell (Alles zusammen)
ost_model_gesamt <- ost_transfer(west_model_gesamt, "Modell_Gesamt")
check_ost_niveau(ost_model_gesamt, "Modell_Gesamt", null_intercept)


# ------------------------------------------------------------------------------
# 5.2 Residuenanalyse Ost (Die Landkarte der unerklärten Reste)
# ------------------------------------------------------------------------------
# Hier visualisieren wir geografisch, wo die Modelle versagen.
# Wir nutzen die Transfer-Modelle und geben das West-Modell als "Elternteil" mit,
# damit die Vorhersage (Offset) korrekt berechnet werden kann.

# --- Modell I (Kultur) ---
calculate_and_export_residuals(west_model_i, "ost", "Modell_I_Gap_Check")
plot_residual_histogram(west_model_i, "ost", "Model_I") # Wie verteilen sich die Fehler?
plot_residual_map(west_model_i, "ost", "Modell_I")

# --- Modell II (Ökonomie) ---
calculate_and_export_residuals(west_model_ii, "ost", "Model_II")
plot_residual_histogram(west_model_ii, "ost", "Model_II")
plot_residual_map(west_model_ii, "ost", "Modell_II")

# --- Modell III (Konflikt) ---
# Hier hoffen wir auf blassere Farben (bessere Erklärungskraft).
calculate_and_export_residuals(west_model_iii, "ost", "Model_III")
plot_residual_histogram(west_model_iii, "ost", "Model_III")
plot_residual_map(west_model_iii, "ost", "Modell_III")

# --- Gesamtmodell ---
calculate_and_export_residuals(west_model_gesamt, "ost", "Model_Gesamt")
plot_residual_histogram(west_model_gesamt, "ost", "Model_Gesamt")
plot_residual_map(west_model_gesamt, "ost", "Modell_Gesamt")

# ------------------------------------------------------------------------------
# 5.3 Binnenanalyse Ost
# ------------------------------------------------------------------------------
# --- Modell I (Kultur) ---
calculate_and_export_residuals(ost_model_i, "ost", "Model_I_Binnen",west_model_i)
plot_residual_histogram(ost_model_i, "ost", "Model_I_Binnen", west_model_i)
plot_residual_map(ost_model_i, "ost", "Modell_I_Binnen", west_model_i)

# --- Modell II (Ökonomie) ---
calculate_and_export_residuals(ost_model_ii, "ost", "Model_II_Binnen",west_model_ii)
plot_residual_histogram(ost_model_ii, "ost", "Model_II_Binnen", west_model_ii)
plot_residual_map(ost_model_ii, "ost", "Modell_II_Binnen", west_model_ii)

# --- Modell III (Konflikt) ---
calculate_and_export_residuals(ost_model_iii, "ost", "Model_III_Binnen",west_model_iii)
plot_residual_histogram(ost_model_iii, "ost", "Model_III_Binnen", west_model_iii)
plot_residual_map(ost_model_iii, "ost", "Modell_III_Binnen", west_model_iii)

# --- Gesamtmodell ---
calculate_and_export_residuals(ost_model_gesamt, "ost", "Model_gesamt_Binnen",west_model_gesamt)
plot_residual_histogram(ost_model_gesamt, "ost", "Model_gesamt_Binnen", west_model_gesamt)
plot_residual_map(ost_model_gesamt, "ost", "Modell_gesamt_Binnen", west_model_gesamt)

# ******************************************************************************
# ENDE DES SKRIPTS
# ******************************************************************************