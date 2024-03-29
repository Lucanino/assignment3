symboltonumber <- function(a) {
  #Funktion jedem Buchstabencode eine Zahl zuzuweisen
  ke <- c(0,1,2,3)
  #Zeilen f�r eine Matrix, jede Zahl entspricht einem Buchstaben
  names <- c("A","C","G","T")
  #Spaltennamen f�r eine Schl�sselmatrix
  key <- matrix(data=ke, nrow=2, ncol=4, byrow=TRUE,dimnames=list(c(1,2),names))
  #eine Matrix mit so vielen Spalten wie Buchstaben wird gebildet
  return(key[1,a])
  #die einem Buchstaben entsprechende Zahl wird ausgegeben
}

quadtodec <- function(b) {
  #Funktion zur Umwandlung eines Tetralzahlvektors in eine Dezimalzahl
  dec <-{}
  #zuerst Vektor f�r einzelne Dezimalzahlen f�r jede Stellt
  for (i in 1:length(b)) {
    #Schleife, die jede Stelle 1er, 4er,16er in Dezimalzahlen umwandelt
    dec[i] <- b[i]*(4^(length(b)-i))
    #allgemeine Formel zur Berechnung der Dezimalzahlen. die Potenz ist hierbei die L�nge des Vektors (bspw. 3 Stellen lang, erste bzw. h�chste Ziffer wird �ber 4^2 berechnet) minus i
  }
  return(sum(dec))
  #Ausgabe der Dezimalzahl aus den Dezimalzahlen f�r jede Ziffer
}

patterntonumber <- function(text) {
  #Funktion, die einen Vektor voller Nucleotidcodes in eine Dezimalzahl umwandelt
  quad <- {}
  #leerer Vektor, der mit tetralzahlen gef�llt wird; eine f�r jeden Buchstaben
  for (i in 1:length(text)) {
    #Schleife, die jeden Nucleotidcode einzeln umwandelt
    quad[i] <- symboltonumber(text[i])
    #ein vektor mit tetralzahl f�r jeden Nucleotidcodebuchstaben 
  }
  number <- quadtodec(quad)
  #Dezimalzahlen aus den Tetralzahlen der Codes
  sum(number)
  #zahlenvektor zu numeric 
}

c("A","G","T")
c("C","T","T","C","T","C","A","C","G","T","A","C","A","A","C","A","A","A","A","T","C")
