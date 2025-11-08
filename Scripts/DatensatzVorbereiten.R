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
      # Erstellt die Variable 'bildung_hoch' als Restwert.
      # Da die drei Kategorien (niedrig, mittel, hoch) 100% ergeben müssen,
      # wird 'bildung_hoch' als 100 minus die anderen beiden Proportionen berechnet.
      bildung_hoch = 100 - bildung_niedrig - bildung_mittel
    )
  
  cat("\nDatensatz vorbereitet\n")
  return(df_neu) # Gibt den Datensatz mit der neuen Spalte zurück
}