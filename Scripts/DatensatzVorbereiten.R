# DatensatzVorbereiten.R

# ************************************************************
# Funktion zur Erstellung abgeleiteter Variablen
# ************************************************************
#' Führt erste notwendige Transformationen am Datensatz durch.
#'
#' Hauptfunktion ist die Erstellung von abgeleiteten Variablen, die für die
#' spätere Analyse benötigt werden, indem bestehende Variablen logisch verknüpft werden.
#' Standardmäßig arbeitet die Funktion mit dem Datensatz 'data'.
#'
#' @param df Data Frame: Der Eingabedatensatz, meist 'data', der vorbereitet werden soll.
#' @return Der Data Frame mit den neu erstellten Variablen.
#' @export
datensatz_vorbereiten <- function(df = data)
{
  df_neu <- df |>
    dplyr::mutate(
      
      # 1. Skalierung der "_prop" Variablen von 0-1 auf 0-100 Prozent
      dplyr::across(
        .cols = dplyr::ends_with("_prop"), 
        .fns = ~ .x * 100
      ), # WICHTIG: Komma trennt die across-Anweisung vom nächsten mutate-Argument
      
      # 2. Erstellt die Variable 'bildung_hoch' als Restwert (benötigt die skalierten Werte)
      bildung_hoch = 100 - bildung_niedrig - bildung_mittel,
      #3 psst die Variable einkoomen an, auf einkommen je Einwohner
      df_neu$einkommen <- df$einkommen/1000
    )
  
  cat("\nDatensatz vorbereitet\n")
  return(df_neu) 
}