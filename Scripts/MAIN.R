# MAIN.R
# ************************************************************
# 1. SETUP: Pakete laden
# ************************************************************
source("Scripts/LoadPackages.R")
# ************************************************************
# 2. Datensatz laden
# ************************************************************
load("Data/Geovisualisierung.RData")
data <- geovisualisierung
# ************************************************************
# 3. Analyse (Deskriptive Statistik)
# 3.1 UNIVARITE
# ************************************************************
PRINT_RESULTS_SINGLE <- TRUE #regelt den Output der einzelnen Tabelle in der Console
PRINT_RESULTS_TOTAL <- TRUE #regelt den Output einer zusammengefassten Tabelle in der Console
PLOT_RESULTS <- TRUE
spalten_von_interesse <- c(
  "afd_prop", 
  "linke_prop" 
)
all_stats_df <- run_descriptive_analysis(spalten_liste = spalten_von_interesse)

#noch neu und muss überarbeitet werden
afd_ost_stats <- calculate_bivariat_descriptive_stats(
  column_name = afd_prop, 
  group_by_var = ost
)
afd_quartile_stats <- calculate_quartile_descriptive_stats(
  column_name = afd_prop, 
  quartile_var = ausl_proz 
)

