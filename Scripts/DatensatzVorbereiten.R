# DatensatzVorbereiten.R

# ************************************************************
# Funktion zur Erstellung abgeleiteter Variablen
# ************************************************************
#' Führt erste notwendige Transformationen am Datensatz durch.

datensatz_vorbereiten_regression <- function(df = data)
{
  df_neu <- df |>
    dplyr::mutate(
      
      # 1. Skalierung der "_prop" Variablen auf 0-1 von 0-100 Prozent
      dplyr::across(
        .cols = dplyr::ends_with("_prop"), 
        .fns = ~ .x / 100
      ),
      # 2. Skalierung des Einkommens, Wohnbestand
      # 3. Erstellung der quadratischen Variabeln
      einkommen = einkommen / 1000,
      einkommen_sq = einkommen^2,
      kath_sq = kath^2,
      evang_sq = evang^2,
      wohnbestand = wohnbestand / 1000,
      wohnbestand_sq = wohnbestand^2
    )
  
  cat("\nDatensatz vorbereitet für Regression\n")
  return(df_neu) 
}
datensatz_vorbereiten <- function() {
  # 1. Daten laden
  data(socec, package = "pradadata")
  data(wahlkreise_shp, package = "pradadata")
  data(elec_results, package = "pradadata")
  
  # 2. Wahlergebnisse: Nur WKR_NR und die Prozent-Anteile behalten
  elec_clean <- elec_results %>%
    filter(parent_district_nr != 99) %>%
    mutate(
      WKR_NR = district_nr,
      afd_prop   = (AfD_3 / votes_3) * 100,
      spd_prop   = (SPD_3 / votes_3) * 100,
      cdu_prop   = (CDU_3 / votes_3) * 100,
      csu_prop   = (CSU_3 / votes_3) * 100,
      linke_prop = (Linke_3 / votes_3) * 100,
      b90_prop   = (Gruene_3 / votes_3) * 100,
      fdp_prop   = (FDP_3 / votes_3) * 100,
      union_prop = (coalesce(CDU_3, CSU_3) / votes_3) * 100
    ) %>%
    # Hier werfen wir die "_3" Spalten raus, indem wir nur WKR_NR und die Props wählen
    select(WKR_NR, ends_with("_prop"))
  
  # 3. Strukturdaten: Rechnen und nur die sauberen Namen auswählen
  new_names <- c(
    state = "V01", WKR_NR = "V02", WKR_NAME = "V03", ausl_proz = "V08",
    gebursaldo = "V10", wandssaldo = "V11", migration = "V19", 
    eigQuote = "V23", wohnbestand = "V25", einkommen = "V26", 
    bip_je_einwohner = "V27", bildung_mittel = "V33", bildung_hoch = "V34",
    sgb2_empf = "V44", sgb2_nichterw = "V45", sgb2_auslaender_prozent = "V46",
    arblQuote_gesamt = "V47", arblQuote_maenner = "V48", arblQuote_frauen = "V49",
    arblQuote_jugend = "V50", arblQuote_senioren = "V51", 
    kath = "V20", evang = "V21"
  )
  
  ost_bundeslaender <- c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", 
                         "Sachsen-Anhalt", "Thüringen", "Berlin")
  
  socec_final <- socec %>%
    mutate(
      alter_bis_24 = V12 + V13,
      alter_25_59 = V14 + V15,
      bildung_niedrig = V31 + V32,
      ost = if_else(V01 %in% ost_bundeslaender, 1, 0)
    ) %>%
    select(
      any_of(new_names), 
      alter_bis_24, alter_25_59, bildung_niedrig, ost
    )
  
  # 4. Zusammenführen
  df_full <- wahlkreise_shp %>%
    inner_join(elec_clean, by = "WKR_NR") %>%
    inner_join(socec_final, by = c("WKR_NR", "WKR_NAME"))
  #if (inherits(df_full, "sf")) {
   # df_full <- sf::st_drop_geometry(df_full)
  #}
  return(df_full)
}
