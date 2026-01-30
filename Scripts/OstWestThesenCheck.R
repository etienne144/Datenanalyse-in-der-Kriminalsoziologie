# ==============================================================================
# Funktion: check_ost_niveau (FINAL)
# ==============================================================================
# ZWECK FÜR DIE ANALYSE: 
# Hier findet der entscheidende "Realitäts-Check" statt.
# Wir vergleichen die harte Realität (Wahlergebnisse Ost) mit der 
# theoretischen Vorhersage unseres West-Modells.
#
# 1. Wir messen den "Fehler" (Intercept): Wie stark unterschätzt das Modell den Osten?
# 2. Wir übersetzen diesen abstrakten Fehler in verständliche Prozentpunkte.
# 3. Wir prüfen, ob der Osten einheitlich ist (Homogenität der Bundesländer).
# ==============================================================================

check_ost_niveau <- function(transfer_model, model_label, null_intercept) {
  
  message(paste0(">>> Erstelle Diagnose-Tabelle für: ", model_label, "..."))
  
  # -------------------------------------------------------
  # SCHRITT 1: Den "Ost-Aufschlag" messen (Fixed Effects)
  # -------------------------------------------------------
  # Der Intercept in diesem Transfermodell ist unser "Restfehler".
  # Er sagt uns: "Nachdem wir alle Strukturdaten berücksichtigt haben, 
  # liegt der Osten immer noch um Wert X höher als der Westen."
  
  fixed_part <- broom.mixed::tidy(transfer_model, effects = "fixed", conf.int = TRUE) %>%
    dplyr::filter(term == "(Intercept)")
  
  # Wir speichern diesen Wert als "Gap" (Lücke zwischen Theorie und Praxis)
  gap_logit <- fixed_part$estimate
  
  # -------------------------------------------------------
  # SCHRITT 2: Die Zerrissenheit prüfen (Random Effects)
  # -------------------------------------------------------
  # Hier schauen wir, ob der "Osten" ein homogener Block ist.
  # Ein hoher Wert (Sigma) bedeutet: Es gibt riesige Unterschiede zwischen 
  # den Bundesländern (z.B. Sachsen vs. Brandenburg), die unser Modell 
  # NICHT erklären konnte.
  
  ran_part <- broom.mixed::tidy(transfer_model, effects = "ran_pars", conf.int = FALSE) %>%
    dplyr::filter(group == "state") %>%
    dplyr::mutate(std.dev = estimate)
  
  std_state <- ran_part$std.dev
  
  # -------------------------------------------------------
  # SCHRITT 3: Die Übersetzung in Prozentwerte (Der Aha-Effekt)
  # -------------------------------------------------------
  # Logits sind schwer zu interpretieren. Wir rechnen sie um.
  
  # A) Die Realität (Der Anker)
  # Wir nehmen den Wert aus dem Nullmodell (ca. -1.416).
  # Das entspricht dem durchschnittlichen Wahlergebnis der AfD im Osten.
  real_prozent <- plogis(null_intercept) * 100
  
  # B) Die Prognose (Was das Modell "dachte")
  # Wenn wir vom echten Wert (Realität) den Fehler (Gap) abziehen,
  # erhalten wir das, was das Modell eigentlich vorhersagen wollte.
  # Formel: Realität - Fehler = Reine Struktur-Prognose
  pred_logit   <- null_intercept - gap_logit
  pred_prozent <- plogis(pred_logit) * 100
  
  # C) Das Ergebnis (Die Lücke in Prozentpunkten)
  # Beispiel: Real 20% - Prognose 10% = 10 Prozentpunkte "Ost-Effekt"
  diff_pp <- real_prozent - pred_prozent
  
  # -------------------------------------------------------
  # SCHRITT 4: Die Beweis-Tabelle erstellen
  # -------------------------------------------------------
  final_table <- data.frame(
    Modell          = model_label,
    
    #Teil 1: Die verständlichen Zahlen (für den Text)
    AfD_Real_Ist    = round(real_prozent, 1),   # Wie stark ist die AfD wirklich?
    AfD_Modell_Soll = round(pred_prozent, 1),   # Was wäre sie "nur" aufgrund der Struktur?
    Fehler_in_PP    = round(diff_pp, 1),        # Wie groß ist der unerklärte Rest?
    
    #Teil 2: Die statistische Absicherung (für den Anhang)
    Gap_Estimate    = round(fixed_part$estimate, NACHKOMMASTELLEN + 1),
    Gap_Signifikant = ifelse(fixed_part$p.value < 0.05, "JA", "NEIN"), # Ist der Fehler Zufall?
    Gap_P_Value     = round(fixed_part$p.value, NACHKOMMASTELLEN + 3),
    
    #Teil 3: Die regionale Ungleichheit
    Sigma_State     = round(std_state, NACHKOMMASTELLEN),     # Ungleichheit zwischen den Ländern
    
    # Konfidenzintervalle (Sicherheitspuffer)
    Conf_Low        = round(fixed_part$conf.low, NACHKOMMASTELLEN + 1),
    Conf_High       = round(fixed_part$conf.high, NACHKOMMASTELLEN + 1)
  )
  
  # -------------------------------------------------------
  # SCHRITT 5: Ergebnisse sichern (Excel-Export)
  # -------------------------------------------------------
  if (exists("IS_TABLE_OUTPUT_ENABLED") && IS_TABLE_OUTPUT_ENABLED) {
    
    dateiname <- paste0("OstWestThese_", model_label, ".csv")
    pfad <- file.path(EXPORT_PFAD_TABELLEN, dateiname)
    write.csv2(final_table, file = pfad, row.names = FALSE)
    message(paste0("   -> Datei gespeichert: ", dateiname))
    
  } else {
    message("   -> Speichern übersprungen (IS_TABLE_OUTPUT_ENABLED = FALSE)")
  }
  
  # Rückgabe der Tabelle, damit sie auch in R angezeigt wird
  return(invisible(final_table))
}