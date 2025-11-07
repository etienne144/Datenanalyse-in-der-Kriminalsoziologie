# MAIN.R

# ************************************************************
# 1. SETUP: Pakete laden----
# ************************************************************
source("Scripts/LoadPackages.R")

# ************************************************************
# 2. Datensatz laden----
# ************************************************************
load("Data/Geovisualisierung.RData")
data <- geovisualisierung

# ************************************************************
# 3. Deskriptive Statistik----
# ************************************************************
PRINT_RESULTS_SINGLE <- TRUE #regelt den Output der einzelnen Tabelle in der Console
PRINT_RESULTS_TOTAL <- TRUE  #regelt den Output einer zusammengefassten Tabelle in der Console
PRINT_PLOT_RESULTS <- TRUE         #regelt Ausgabe der Visualisierung
HISTOGRAM_BINS_FAKTOR <- 4

# ************************************************************
# 3.1 UNIVARITE Analyse----
# ************************************************************
spalten_von_interesse <- c(
  "afd_prop", 
  "linke_prop",
  "ausl_proz"
)
all_stats_df <- run_descriptive_analysis(spalten_liste = spalten_von_interesse)

# ************************************************************
# 3.1.1 UNIVARITE Visualisierung----
# ************************************************************
run_univariate_plotting(spalten_liste = spalten_von_interesse)


# ************************************************************
# 3.1 BIVARITE Analyse----
# ************************************************************
#noch neu und muss überarbeitet werden
afd_ost_stats <- calculate_bivariat_descriptive_stats(
  column_name = linke_prop, 
  group_by_var = ost
)
afd_quartile_stats <- calculate_quartile_descriptive_stats(
  column_name = bip_je_einwohner, 
  quartile_var = ausl_proz 
)

