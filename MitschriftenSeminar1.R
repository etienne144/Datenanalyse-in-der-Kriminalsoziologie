#-----------------------------------------------------------
# MITTSCHRIFT: EINFÜHRUNG IN R UND RSTUDIO (BLOCK 1)
#-----------------------------------------------------------

# Hallo ihr Beiden,
# In diesem File findet ihr meine Mitschriften aus dem ersten Block,
# welche ich zu Hause für euch überarbeitet habe.

## 1) Naming Rules (Namenskonventionen)

# Wir verwenden in R die Konvention des "Snake Case".
# Dies bedeutet, dass Variablen mit Kleinbuchstaben geschrieben 
# und Wörter mit Unterstrich getrennt werden. Bspw.: anzahl_der_morde


## 2) R als Taschenrechner und Variablenzuweisung

# Zuweisungsoperator:
# Um Variablen zu definieren, wird '<-' verwendet. 
# Dies ist das bevorzugte R-Äquivalent zu einem '='.

a <- 1
b <- 2

# Datentypen:
# der Datentyp muss nicht definiert werden. 
# Er wird automatisch erkannt, sollte aber im Hinterkopf behalten werden.Nicht das wir 
# am Ende nicht passende Datentype miteinader versuchen zu verrechnen

# numeric/double = Dezimalzahl (der Standard-Zahlentyp)
# integer = Ganze Zahl
# character = Text (String)
# bool/logical = Wahrheitswert (TRUE oder FALSE)

#Wir benennen Variablen kurz und klar, ohne den Typ im Namen anzugeben, aber so das er logisch erschließbar ist:
anzahl_der_morde <-1  # Typ: integer, da es eine Anzahl ist und somit nur ganze Zahlen als Wert haben kann
namen_der_personen <- "Max" # Typ: character
arbeitslosenquote <- 7.5 #ist dann ein double, da es durch Quote durchaus Kommastellen haben kann


## 3) Funktionen

# Funktionen sind Code-Blöcke, die eine Aufgabe erfüllen.
# Sie besitzen einen Namen und Argumente (Input-Werte).
# Eine Funktion und einen Output

# Funktionen kapseln den Code, der mit den Werten arbeitet, in einem benannten Block. 
#Anstatt denselben Code mehrmals zu kopieren (z.B. den Prüf- und Installationscode für ein Paket), 
#definieren wir die Funktion (Lade) nur einmal. Danach rufen wir sie mit ihrem Namen auf, was viel kürzer ist. 
#Dies spart Zeit und macht den Code schlank.

# Es gibt vorgefertigte Funktionen (sum()) und benutzerdefinierte Funktionen.

# Beispiel für eine benutzerdefinierte Funktion:
# Funktionsname             Inputs
addiere_zahlen <- function(input_A, input_B)
{
  # Der Funktionskörper arbeitet mit den Input-Werten
  ergebnis <- input_A + input_B
  
  # return() gibt das Ergebnis explizit zurück und beendet die Funktion
  return(ergebnis)
}

# Funktion werden dann von außerhalb aufgerufen und ihr Output kann auffangen werden:
summe <- addiere_zahlen(a, b) # summe ist nun 3

# Praktisch, es gibt bereits eine Funktion, die die Aufgabe des addieren von Zahlen übernimmt
# wird ein Fragezeichen vor eine Funktion gesetzt, bekommen wir eine Erklärung der Funktion
c <- sum(a, b)

# Vorgefertigte Funktionen (wie sum()) sind zwar extrem schnell und effizient,
# aber sie sind nur für Standardaufgaben geeignet. Wenn wir eine spezielle Logik 
# abbilden wollen, die mehrere Schritte kombiniert, müssen wir eine eigene Funktion schreiben.



#-----------------------------------------------------------
# Anwendung DER FUNKTIONEN UND PAKETVERWALTUNG
#-----------------------------------------------------------

# Beispiel: Die notwendige Abfolge "prüfen, ob Paket da -> installieren -> laden" existiert nicht als Standardfunktion.

# Die folgende, komplexere Funktion vereinfacht das Herunterladen und Laden von Paketen.
# Sie verhindert, dass man die 'install.packages'-Zeile manuell mit # auskommentieren muss,
# wenn das Projekt auf einem neuen PC geöffnet wird.

Lade <- function(paket_name) 
{
  # 1. Prüfen und Installieren, falls notwendig
  # Die 'if'-Bedingung fragt: IST das Paket NICHT installiert?
  if (!requireNamespace(paket_name, quietly = TRUE)) {
    message(paste("Das Paket '", paket_name, "' wird installiert...", sep = ""))
    install.packages(paket_name)
  }
  
  # 2. Paket laden
  # Dies zieht das installierte Paket in die aktuelle R-Sitzung.
  library(paket_name, character.only = TRUE)
  message(paste("Das Paket '", paket_name, "' wurde geladen.", sep = ""))
}

# Aufruf der Lade-Funktion
Lade("here")

# Erklärung der Funktion:
# Die Funktion wirkt komplex, weil sie neue, unbekannte Befehle verwendet. 
# Sie beruht aber auf den zwei grundlegenden Befehlen zur Paketverwaltung:

# Mit 'install.packages("here")' wird das Paket einmalig auf dem Rechner installiert. 
# Der obere Teil der Funktion (die 'if'-Abfrage) prüft, ob dieser Installationsschritt 
# bereits erfolgt ist. Wenn ja, wird der Installationscode übersprungen.

# Im zweiten Schritt wird dann das installierte Paket in dieses Projekt geladen. 
# Dieser Befehl muss jedes Mal mitlaufen, wenn das Paket verwendet werden soll:
# library(here) 


# DATEN-IMPORT (unter Verwendung des geladenen 'here'-Pakets)
# Der empfohlene Weg, um die Datei zu laden:
load("Data/pol_mord.RData") 
# Anmerkung: Der Ordner "Data" befindet sich im Projektordner und enthält die Datei "pol_mord.RData".


# ERSTE DATENÜBERPRÜFUNG
# Es existieren bereits praktische Funktionen, um sich den geladenen Datensatz anzuschauen:
str(pol_mord)  # Zeigt die Struktur (Variablentypen und Anzahl der Beobachtungen).
head(pol_mord) # Zeigt die ersten 6 Zeilen des Datensatzes an.
View(pol_mord) # Zeigt den gesamten Datensatz in einer tabellarischen Ansicht.

