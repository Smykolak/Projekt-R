#Projekt R - Wypadki

#Użyte biblioteki:
install.packages("ggplot2") #Umożliwa tworzenie wykresów
library(ggplot2) 


wypadki_dane <- read.csv("wypadki_20232024.csv") #Wczytanie pliku csv

#Rozdzielenie ramki danych na poszczególne elementy
wojewodztwo <- wypadki_dane$Wojewodztwo
typ_podmiotu <- wypadki_dane$TypPodmiotu
rodzaj_wypadku <- wypadki_dane$RodzajWypadku
przyczyna <- wypadki_dane$RodzajWypadku
miejsce <- wypadki_dane$MiejsceWypadku
rodzaj_zajec <- wypadki_dane$RodzajZajec
liczba_wypadkow <- wypadki_dane$Liczba.Wypadkow

#Porównanie ilości wypadków w podziale na województwa
suma_wojewodztwa <- aggregate(liczba_wypadkow ~ wojewodztwo, FUN = sum)
#Wykres przedstawiający ilość wypadków w poszczególnych województwach
ggplot(suma_wojewodztwa, aes(x = wojewodztwo, y = liczba_wypadkow)) + #Wybór danych oraz podział na osie X i Y
geom_bar(stat = "identity", fill = "lightblue") + #Kolumny, typ oraz kolor
geom_text(aes(label=liczba_wypadkow, angle = 90, hjust = 1)) + # Etykiety liczby wypadków
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #Etykiety Województw
labs(title = "Liczba wypadków w województwach", x = "Województwo", y = "Liczba wypadków") #Tytuł oraz nazwy osi

