fasterfrequentwords <- function(text,k) {
  #Funktion von Nucleotidfolge und Länge der gesuchten Frequenz k
  frequentpatterns <- matrix(data = c(1:k),ncol = k)
  #matrix als vertikale Liste für häufigste patterns
 
  #Erstellung von Liste von häufigsten patterns
  maxcount <- max(frequencyarray)
  #es wird ermittelt, welche stelle (als integer) den höchsten Wert darstellt
  for (i in 1:((4^k))) {
    #Schleife, die frequencyarray entlanggeht
    if (frequencyarray[i] == maxcount) pattern <- numbertopattern(i-1,k) 
    #wenn an stelle i pattern höchster Häufigkeit ist, dann wird pattern aus nummer bei stelle i hergestellt
    rbind(frequentpatterns,pattern)
    #Vektor pattern wird an bisherige häufigste patterns angehangen
  }
  unique(frequentpatterns)
  #mehrfache Zeilen werden einzeln gemacht
  return(frequentpatterns)
  #Werte werden ausgegeben
}
text <- c("A","A","G","C","A","A","A","G","G","T","G","G","G")
k <- 2
frequencyarray <- c(3,0,2,0,1,0,0,0,0,1,3,0,0,0,1,0)