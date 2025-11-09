# VariablenMapping.R
# ************************************************************
# ZWECK: Definiert die Klartextnamen (Labels) für alle Variablen und Achsen.
#        Dient als zentrales Nachschlagewerk (Mapping) für Plotting und Berichterstellung.
# ************************************************************

# 1. Klartextnamen für alle metrischen Zielvariablen (Target-Variablen)
# Wird verwendet für: Y-Achsentitel und für die Beschriftung der X-Achse der Boxplots.
KLARNAMEN <- c(
  "bildung_niedrig" = "niedrig",
  "bildung_mittel" = "mittel",
  "bildung_hoch" = "hoch",
  "afd_prop" = "AfD",
  "linke_prop" = "Die Linke",
  "ausl_proz" = "Ausländerquote",
  "einkommen" = "Durchschn. Einkommen",
  "arblQuote_gesamt" = "gesamt",
  "arblQuote_frauen" = "Frauen",
  "arblQuote_maenner" = "Männer",
  "arblQuote_jugend" = "Jugend",
  "arblQuote_senioren" = "Senioren"
)

# 2. Klartextnamen für alle Gruppierungsvariablen (Grouping-Variablen)
# Wird verwendet für: Legendentitel (fill)
GROUP_KLARNAMEN <- c(
  "ost" = "Ost-/Westdeutschland"

)
GRUPPEN_WERTE_KLARTEXT <- list(
  "ost" = c("0" = "Westdeutschland", "1" = "Ostdeutschland")
)

# 3. Einheiten-Mapping
# Wird verwendet für: Y-Achsen-Einheit
EINHEITEN_MAPPING <- c(
  "einkommen" = "in Euro (€)"
)