#Importowanie danych

wypadki<-read.csv("wypadki.csv")
wojewodztwaMiejsce<-data.frame(wypadki[c(1:4,13)])
miejsceRodzaj<-data.frame(wypadki[c(3:6,13)])
miejscePrzyczyna<-data.frame(wypadki[c(3,4,7,8,13)])

#Obrabrianie danych
