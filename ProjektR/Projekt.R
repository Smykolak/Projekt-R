#Projekt R - Wypadki

#Użyte biblioteki:
install.packages("ggplot2") #Umożliwa tworzenie wykresów
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("devtools")

library(ggplot2) 
library(knitr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

wypadki_dane <- read.csv("wypadki.csv") #Wczytanie pliku csv

#Sumujemy wszystkie wypadki według województw
suma_wojewodztwa <- aggregate(Liczba.Wypadkow ~ Wojewodztwo, data = wypadki_dane, FUN = sum) #Zliczamy liczbe wypadków według kolumny Wojewodztwo
#Wykres przedstawiający ilość wypadków w poszczególnych województwach
ggplot(suma_wojewodztwa, aes(x = reorder(Wojewodztwo, -Liczba.Wypadkow), y = Liczba.Wypadkow)) + #Sortowanie malejąco
geom_bar(stat = "identity", fill = "lightblue") + #Kolumny, typ oraz kolor
geom_text(aes(label=Liczba.Wypadkow), angle = 90, hjust = 1, vjust = 0.5) + #Etykiety liczby wypadków
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #Etykiety Województw
labs(title = "Liczba wypadków w województwach", x = "Województwo", y = "Liczba wypadków") #Tytuł oraz nazwy osi


#TOP 10 PRZYCZYN WYPADKÓW w woj. Śląskim
slaskie_dane <- subset(wypadki_dane, Wojewodztwo == "ŚLĄSKIE") #Pobieramy dane tylko z województwa śląskiego
top_przyczyny <- aggregate(Liczba.Wypadkow ~ PrzyczynaWypadku, data = slaskie_dane, FUN = sum) #Zliczamy według przyczyny
top10_przyczyny <- head(top_przyczyny[order(-top_przyczyny$Liczba.Wypadkow), ], 10) #Bierzemy te z najwiekszą ilością wypadków, 10 od góry
top10_przyczyny$ID <- seq_len(nrow(top10_przyczyny)) #Tworzymy kolumne ID z numerami 1 do 10
top10_przyczyny <- top10_przyczyny[, c("ID", "PrzyczynaWypadku", "Liczba.Wypadkow")] #Porządkujemy kolumny

print(top10_przyczyny, row.names = F)
# Lub
kable(top10_przyczyny,row.names = F, caption = "Top 10 przyczyn wypadków – woj. śląskie", 
      col.names = c("ID", "Przyczyna", "Ilość"))




#Wykres Kołowy Miejsce Wypadku
# Agregacja liczby wypadków i obliczenie udziałów procentowych
suma_miejsce <- wypadki_dane %>%
  group_by(MiejsceWypadku) %>%
  summarise(Liczba.Wypadkow = sum(Liczba.Wypadkow), .groups = "drop") %>%
  mutate(Procent = Liczba.Wypadkow / sum(Liczba.Wypadkow) * 100)

# Podział na duże i małe udziały (<10.5%)
duze <- filter(suma_miejsce, Procent >= 10.5)
male <- filter(suma_miejsce, Procent < 10.5)

# Tworzenie rekordu "inne"
inne_row <- data.frame(
  MiejsceWypadku = "inne",
  Liczba.Wypadkow = sum(male$Liczba.Wypadkow),
  Procent = sum(male$Procent))

# Połączenie danych i sortowanie: najpierw malejąco, potem "inne" na końcu
dane_do_wykresu <- bind_rows(duze, inne_row) %>%
  arrange(desc(Liczba.Wypadkow)) %>%
  mutate(MiejsceWypadku = factor(MiejsceWypadku, levels = c(setdiff(MiejsceWypadku, "inne"), "inne")))

# Wykres kołowy
ggplot(dane_do_wykresu, aes(x = "", y = Procent, fill = MiejsceWypadku)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Procent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Rozkład procentowy miejsc wypadków", fill = "Miejsce wypadku") +
  theme_void() +
  theme(legend.text = element_text(size = 9))


#Mapa Polski
# Filtruj dane
umyślne_typy <- c("działania umyślne ucznia", "działania umyślne innej osoby", "umyślne uderzenie")

umyślne_dane <- wypadki_dane %>%
  filter(RodzajWypadku %in% umyślne_typy) %>%
  group_by(Wojewodztwo) %>%
  summarise(UmyslneWypadki = sum(Liczba.Wypadkow), .groups = "drop")

# Dopasuj nazwy do mapy
umyślne_dane$Wojewodztwo <- tolower(umyślne_dane$Wojewodztwo)
umyślne_dane$Wojewodztwo <- tools::toTitleCase(umyślne_dane$Wojewodztwo)

# Łączymy dane z mapą
mapa_z_danymi <- merge(polska_mapa, umyślne_dane, by.x = "name_alt", by.y = "Wojewodztwo", all.x = TRUE)
mapa_z_danymi$UmyslneWypadki[is.na(mapa_z_danymi$UmyslneWypadki)] <- 0

# Rysuj mapę
ggplot(mapa_z_danymi) +
  geom_sf(aes(fill = UmyslneWypadki), color = "white") +
  scale_fill_gradient(low = "#e0f3f8", high = "#08306b", name = "Umyślne wypadki") +
  labs(title = "Umyślne wypadki w polskich województwach") +
  theme_minimal()


