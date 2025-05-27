#Projekt R - Wypadki

#Użyte biblioteki:
install.packages("ggplot2") #Umożliwa tworzenie wykresów
library(ggplot2) 
library(knitr)
library(dplyr)

wypadki_dane <- read.csv("wypadki.csv") #Wczytanie pliku csv

#Rozdzielenie ramki danych na poszczególne elementy
wojewodztwo <- data.frame(wypadki_dane$Wojewodztwo)
typ_podmiotu <-data.frame( wypadki_dane$TypPodmiotu)
rodzaj_wypadku <- data.frame(wypadki_dane$RodzajWypadku)
przyczyna <- data.frame(wypadki_dane$RodzajWypadku)
miejsce <- data.frame(wypadki_dane$MiejsceWypadku)
rodzaj_zajec <- data.frame(wypadki_dane$RodzajZajec)
liczba_wypadkow <- data.frame(wypadki_dane$Liczba.Wypadkow)

#Porównanie ilości wypadków w podziale na województwa
suma_wojewodztwa <- aggregate(Liczba.Wypadkow ~ Wojewodztwo, data = wypadki_dane, FUN = sum)
#Wykres przedstawiający ilość wypadków w poszczególnych województwach
ggplot(suma_wojewodztwa, aes(x = reorder(Wojewodztwo, -Liczba.Wypadkow), y = Liczba.Wypadkow)) + #Sortowanie malejąco
geom_bar(stat = "identity", fill = "lightblue") + #Kolumny, typ oraz kolor
geom_text(aes(label=Liczba.Wypadkow), angle = 90, hjust = 1, vjust = 0.5) + #Etykiety liczby wypadków
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #Etykiety Województw
labs(title = "Liczba wypadków w województwach", x = "Województwo", y = "Liczba wypadków") #Tytuł oraz nazwy osi

#TOP 10 PRZYCZYN WYPADKÓW w woj. Śląskim
slaskie_dane <- subset(wypadki_dane, Wojewodztwo == "ŚLĄSKIE")
top_przyczyny <- aggregate(Liczba.Wypadkow ~ PrzyczynaWypadku, data = slaskie_dane, FUN = sum)
top10_przyczyny <- head(top_przyczyny[order(-top_przyczyny$Liczba.Wypadkow), ], 10)
top10_przyczyny$ID <- seq_len(nrow(top10_przyczyny))
top10_przyczyny <- top10_przyczyny[, c("ID", "PrzyczynaWypadku", "Liczba.Wypadkow")]

print(top10_przyczyny, row.names = F)
# Lub
kable(top10_przyczyny,row.names = F, caption = "Top 10 przyczyn wypadków – woj. śląskie", 
      col.names = c("ID", "Przyczyna", "Ilość"))

#Wykres Kołowy Miejsce Wypadku
# Agregacja liczby wypadków według miejsca wypadku
suma_miejsce <- aggregate(Liczba.Wypadkow ~ MiejsceWypadku, data = wypadki_dane, FUN = sum)

# Dodanie kolumny z procentowym udziałem
suma_miejsce$Procent <- suma_miejsce$Liczba.Wypadkow / sum(suma_miejsce$Liczba.Wypadkow) * 100

# Agregacja liczby wypadków według miejsca wypadku
suma_miejsce <- aggregate(Liczba.Wypadkow ~ MiejsceWypadku, data = wypadki_dane, FUN = sum)

# Obliczenie udziałów procentowych
suma_miejsce$Procent <- suma_miejsce$Liczba.Wypadkow / sum(suma_miejsce$Liczba.Wypadkow) * 100

# Podział na duże i małe udziały (<1%)
duze <- suma_miejsce %>% filter(Procent >= 5)
male <- suma_miejsce %>% filter(Procent < 5)

# Sumujemy "małe" jako "Inne"
inne_sum <- sum(male$Liczba.Wypadkow)
inne_procent <- sum(male$Procent)

# Tworzymy nowy wiersz dla "Inne"
inne_row <- data.frame(MiejsceWypadku = "Pozostałe != inne", Liczba.Wypadkow = inne_sum, Procent = inne_procent)

# Łączymy z "dużymi"
dane_do_wykresu <- rbind(duze, inne_row)

# Wykres kołowy, ale bez etykiety procentowej dla "Inne"
ggplot(dane_do_wykresu, aes(x = "", y = Procent, fill = MiejsceWypadku)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(data = subset(dane_do_wykresu, MiejsceWypadku != "Inne"),
            aes(label = paste0(round(Procent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Rozkład procentowy miejsc wypadków", fill = "Miejsce wypadku") +
  theme_void() +
  theme(legend.text = element_text(size = 9))
