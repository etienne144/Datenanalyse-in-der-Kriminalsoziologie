# ==============================================================================
# Funktion: ost_transfer
# ==============================================================================
# ZWECK:
# 1. Nimmt ein West-Modell und berechnet, was es für den Osten vorhersagen würde.
# 2. Rechnet ein neues Modell ("Transfer-Modell"), das prüft, wie stark 
#    die Realität davon abweicht (mit Offset).
# 3. Gibt das fertige Modell zurück
# ==============================================================================
ost_transfer <- function(west_model, model_label) {
  
  message(paste0("\n>>> Starte Ost-Transfer für: ", model_label, "..."))
  
  # 1. Daten filtern (Nur Osten)
  data_ost <- data %>% 
    dplyr::filter(ost == 1)
  
  # 2. Vorhersage berechnen
  data_ost$linear_predictor <- predict(west_model, newdata = data_ost, type = "link",allow.new.levels = TRUE)
  
  # 3. Das Transfer-Modell rechnen.
  transfer_model <- glmmTMB::glmmTMB(
    afd_prop ~ offset(linear_predictor) + (1 | state),
    family = glmmTMB::beta_family(link = "logit"),
    data = data_ost
  )
  
  # 4. Das Modell zurückgeben
  return(transfer_model)
}

# ==============================================================================
# Funktion: calculate_null_model
# ==============================================================================
# ZWECK:
# Berechnet das Referenzmodell für den Osten (ohne jegliche Strukturdaten).
# Dient als Benchmark für alle Vergleiche.
# ==============================================================================

calculate_null_model <- function() {
  
  message(">>> Berechne Null-Modell (Benchmark für den Osten)...")
  
  # 1. Daten filtern
  data_ost <- data %>% dplyr::filter(ost == 1)
  
  # 2. Modell rechnen
  null_model <- glmmTMB::glmmTMB(
    afd_prop ~ (1 | state),
    family = glmmTMB::beta_family(link = "logit"),
    data = data_ost
  )
  
  return(null_model)
}