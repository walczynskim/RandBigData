library(PogromcyDanych)
library(dplyr)

wybrane.auta <- function(marka = character(0), model = character(0),
              rodzaj.paliwa = character(0), moc.silnika = numeric(0),
              rok.produkcji= numeric(0), przebieg= numeric(0)) {
   
   temp <- tbl_df(auta2012)
   
   if (length(marka) == 1)
      temp <- temp %>% filter(Marka == marka)
   if (length(model) == 1)
      temp <- temp %>% filter(Model == model)
   if (length(rodzaj.paliwa) == 1)
      temp <- temp %>% filter(Rodzaj.paliwa == rodzaj.paliwa)
   if (length(moc.silnika) == 1)
      temp <- temp %>% filter(KM >= moc.silnika)
   if (length(rok.produkcji) == 1)
      temp <- temp %>% filter(Rok.produkcji >= rok.produkcji)
   if (length(przebieg) == 1)
      temp <- temp %>% filter(Przebieg.w.km <= przebieg)
   if (nrow(temp)>0) {
    temp<-temp %>%  summarise(Minimalna.cena = min(Cena.w.PLN, na.rm=TRUE),
                Q1.ceny = quantile(Cena.w.PLN, 0.25, na.rm=TRUE),
                Mediana.ceny = median(Cena.w.PLN, na.rm=TRUE),
                Srednia.cena = mean(Cena.w.PLN, na.rm=TRUE),
                Q3.ceny = quantile(Cena.w.PLN, 0.75, na.rm=TRUE),
                Maksymalna.cena = max(Cena.w.PLN, na.rm=TRUE)
                )
    return(temp)
   } else {
      return("Nie znaleziono aut spelniajacych podane wymagania")
   }
   
}
wybrane.auta()
wybrane.auta("Toyota", "Corolla", 'benzyna', 116, 1000, 10)
wybrane.auta(model="Carens", rodzaj.paliwa = 'benzyna', moc.silnika = 116, 
    rok.produkcji = 2000, przebieg = 10000)
wybrane.auta(marka = "Toyota", model = "Corolla", rodzaj.paliwa = 'benzyna', 
    rok.produkcji = 2010, przebieg = 10000)
wybrane.auta(marka = "Toyota", model = "Corolla", rodzaj.paliwa = 'benzyna', 
    rok.produkcji = 2010)
wybrane.auta(przebieg = 1)


