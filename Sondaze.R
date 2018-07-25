# install.packages('tidyverse')
# install.packages('rvest')
# install.packages('lubridate')
# install.packages('forecast')


library(tidyverse)
library(rvest)
library(lubridate)
library(forecast)

#reset pamięci
rm(list=ls())

url <- 'https://docs.google.com/spreadsheets/d/1P9PG5mcbaIeuO9v_VE5pv6U4T2zyiRiFK_r8jVksTyk/htmlembed?single=true&gid=0&range=a10:o400&widget=false&chrome=false'
# download.file(url, destfile = "C:\\R - materiały")
page <- read_html(url)

# pobieramy tabelę z treści strony
sondaze <- page %>% html_nodes("table") %>%  html_table(fill = TRUE) %>% .[[1]]

# potrzebujemy tabeli bez górnych wierszy
sondaze <- sondaze[4:nrow(sondaze),c(2,4,5,6,8:16)]

# nadajemy nazwy kolumnom
colnames(sondaze) <- c("Osrodek", "Publikacja", "Metoda", "UwzgNiezdecydowani", "PiS", "PO", "K15", "N", "PSL", "SLD", "Razem", "Wolnosc", "NieZdec")

# poprawiamy typ daty
sondaze$Publikacja <- dmy(sondaze$Publikacja)

# zmieniamy przecinki na kropki, a później ciąg na liczbę
for(nc in 5:13) { sondaze[,nc] <- as.numeric(gsub(",", ".", sondaze[,nc], fixed = TRUE)) }

#Trend poparcia dla PIS
sondaze %>%
  ggplot() +
  geom_point(aes(Publikacja, NieZdec, group = Osrodek, color = Osrodek)) +
  geom_smooth(aes(Publikacja, NieZdec))

#Który ośrodek badawczy przygotował najwięcej sondaży
sondaze %>%
  count(Osrodek, Metoda) %>%
  ggplot() + 
  geom_tile(aes(Metoda, Osrodek, fill = n), color = "gray80")

#CATI ranking
sondaze %>% 
filter(Metoda == "CATI") %>%
  count(Osrodek, sort = TRUE)

sondaze %>% filter(Metoda == "CAPI") %>% count(Osrodek, sort = TRUE)


# Wszystkie sondaże
sondaze %>%
  # tylko metoda CATI
  filter(Metoda == "CATI") %>%
  select(-Metoda, -UwzgNiezdecydowani) %>%
  # tabela z szerokiej na długą - na potrzeby wykresu
  gather(Partia, Procent, -Osrodek, -Publikacja) %>%
  ggplot() +
  geom_point(aes(Publikacja, Procent, color = Osrodek), show.legend = FALSE) +
  geom_smooth(aes(Publikacja, Procent, color = Partia), method = "loess", show.legend = FALSE) +
  facet_wrap(~Partia, scales = "free_y")

#Jaka będzie przyszłość?

sondaze_agr <- sondaze %>%
  select(-NieZdec, -Metoda, -UwzgNiezdecydowani) %>%
  rowwise() %>%
  # dodajemy sumę poparcia dla "opozycji"
  mutate(Opozycja = sum(PO, N, PSL, SLD, Razem, na.rm = TRUE)) %>%
  ungroup() %>%
  # potrzebujemy długiej tabeli
  gather(Partia, Procent, -Osrodek, -Publikacja) %>%
  # będziemy uśredniać po kolejnych miesiącach
  mutate(miesiac = make_date(year(Publikacja), month(Publikacja), 1)) %>%
  group_by(miesiac, Partia) %>%
  summarise(Proc = mean(Procent)) %>%
  ungroup()
###########

sondaze_agr %>%
  filter(Partia %in% c("PiS", "Opozycja", "K15", "Wolnosc")) %>%
  ggplot() +
  geom_point(aes(miesiac, Proc, color = Partia), alpha = 0.3) +
  geom_smooth(aes(miesiac, Proc, color = Partia), size = 2, method = "loess") +
  scale_x_date(date_breaks = "3 months", date_labels = "%m'%y") +
  scale_y_continuous(breaks = seq(0, 60, 5))

#########

# potrzebujemy na powrót szerokiej tabeli
sondaze_agr <- spread(sondaze_agr, Partia, Proc)

# uzupełnienie brakujących notowań dla każdej z partii wartościami średnimi
# braki są tylko w trzech przypadkach:
sondaze_agr[is.na(sondaze_agr$N),"N"] <- mean(sondaze_agr$N, na.rm = TRUE)
sondaze_agr[is.na(sondaze_agr$Razem),"Razem"] <- mean(sondaze_agr$Razem, na.rm = TRUE)
sondaze_agr[is.na(sondaze_agr$Wolnosc),"Wolnosc"] <- mean(sondaze_agr$Wolnosc, na.rm = TRUE)

# na ile miesięcy w przód robimy predykcję?
n_okresow <- 6

# żeby nie było to więcej niż połowa danych historycznych
n_okresow <- ifelse(n_okresow > nrow(sondaze_agr)/2, nrow(sondaze_agr)/2, n_okresow)

# generujemy "nazwy" miesięcy
sondaz_pred <- as.character(make_date(2018, 07, 1) + months(0:24)) %>% substr(1, 7) %>% .[1:n_okresow]


for(i in 2:ncol(sondaze_agr)) {
  # szereg czasowy dla konkretnej partii
  ts <- ts(sondaze_agr[,i], start = c(2015, 10), frequency = 12)
  
  # model
  model <- Arima(ts, order = c(2, 0, 0))
  
  # predykcje
  pred <- forecast(model, h = n_okresow)
  
  # łączymy w całość   
  sondaz_pred <- cbind(sondaz_pred, as.numeric(pred$mean))
}

# zmiana typu części z predykcjami
sondaz_pred <- as_tibble(sondaz_pred)

# nazwy kolumn jak w danych historycznych
colnames(sondaz_pred) <- colnames(sondaze_agr)

# poprawiamy typy danych w kolumnach i oznaczamy, że są to predykcje
sondaz_pred <- sondaz_pred %>%
  mutate(miesiac = make_date(substr(miesiac, 1, 4), substr(miesiac, 6, 7), 1)) %>%
  mutate_at(2:ncol(sondaz_pred), as.numeric) %>%
  mutate(type = "predykcja")

# łączymy historię z przyszłością
sondaze_pred_all <- bind_rows(sondaze_agr %>% mutate(type = "sondaż"), # oznaczamy, że to była historia
                              sondaz_pred)

sondaze_pred_all %>%
  gather(Partia, poparcie, -miesiac, -type) %>%
  ggplot() +
  geom_col(aes(miesiac, poparcie, fill = type)) +
  facet_wrap(~Partia, scales = "free_y")

sondaze_pred_all %>%
  gather(Partia, poparcie, -miesiac, -type) %>%
  filter(Partia %in% c("PiS", "Opozycja", "K15", "Wolnosc")) %>%
  ggplot() +
  geom_line(aes(miesiac, poparcie, color = Partia, linetype = type == "predykcja"), size = 2, show.legend = FALSE) +
  scale_x_date(date_breaks = "2 months", date_labels = "%m'%y") +
  scale_y_continuous(breaks = seq(0, 60, 5))