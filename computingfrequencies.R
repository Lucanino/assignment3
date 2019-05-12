computingfrequencies <- function(text,k) {
  #Funktion die eine Liste aller möglichen patterns un deren Häufigkeit ausgibt
  frequencyarray <- {}
  #Vektor frequencyrray beliebiger Länge wird erzeugt
  for (i in 1: (4^(k))) {
    #Schleife, die ein array so lang wie alle möglichen Nucleotidkombinationen ist, bildet
    frequencyarray[i] <- 0
    #Stellen werden auf null gesetzt
  } 
  for (i in 1:length(text)-k) {
    #Schleife, die die Textsequenz entlanggeht
    pattern <- text[i:(i+k-1)]
    #pattern wird als Sequenz von Länge k bei Punkt i definiert
    j <- patterntonumber(pattern)
    #i-abhängige Sequenz namens pattern wird in eine (Dezimal-) Zahl umgewandelt, die die möglichen patterns nummeriert
    frequencyarray[j+1] <-frequencyarray[j]+1
    #Wert an Stelle von dezimalzahl j+1 (notwendig, weilj = 0 für A... steht aber die erste Stelle eines Vektors immer 1 ist) wird um 1 erhöht, weil entsprechendes pattern vorhanden ist
  }
  return(frequencyarray)
  #Ausgabe von Vektor frequencyarray
}

sample <- c("A","C","G","C","G","G","C","T","C","T","G","A","A","A")
text <-sample
k <-2
extra <- c("A","C","T","T","C","G","C","C","T","A","A","G","T","C","A","T","T","T","A","T","C","C","C","G","T","G","G","T","A","C","G","A","C","G","C","T","C","C","C","T","T","A","C","A","G","T","C","T","T","A","T","A","T","C","C","C","G","G","T","A","T","A","T","A","C","G","C","A","G","A","A","A","T","G","C","C","T","A","C","G","T","C","C","C","C","T","C","G","T","C","C","C","A","C","A","C","A","C","C","A","G","G","G","A","A","G","C","T","G","A","A","A","T","C","G","C","T","C","A","T","C","T","A","C","T","A","T","G","C","G","T","G","T","A","C","T","T","C","C","G","G","A","C","G","A","A","A","T","C","G","T","C","G","T","C","G","G","C","T","T","C","T","G","T","C","T","G","G","C","G","C","T","G","G","A","G","A","T","C","C","G","G","G","C","T","T","C","T","T","G","A","G","G","G","A","C","A","C","A","C","C","C","A","T","T","A","T","G","A","C","C","G","T","T","A","C","A","G","G","A","C","T","T","A","C","A","A","C","T","A","C","T","C","T","G","A","G","C","A","A","T","G","A","T","G","G","T","G","C","T","C","T","G","T","A","A","C","G","A","A","C","A","A","A","C","G","C","A","C","T","C","A","C","C","T","C","T","G",TTTCCTGTATGACATCCTCAAATGGATCGACCGTGATGTACTGAGCGAATAAGTGCGGATTACATTTATAGTCAGCTACATTTATTCGCCGCTCGGAGCAGAGTATAATGAATTTATACCACTTGTTAGACTCCTTCTCGCATTTAGCCCCTACCGCAAGTCGGAGCGTTGGGGTGCAATAGAGTTTTCAGTATCTACGTACCGTTAAGTCTCTCGCGTTCTTTCAGCAGGCATCAATATGTTGCTTGCTGTGGGGTCGGGTGGGGCGGAGAGCCAATAAAGTGCATCGGAATTGGCTGCCCTCCTACGAATCCGCAAGATGCGGTGATGCTACGTGATTATGACTACTAGCTTAGTCCC)
k <-6