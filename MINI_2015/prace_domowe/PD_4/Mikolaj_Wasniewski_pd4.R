
library(PogromcyDanych)
library(dplyr)

wybrane.auta <- function(marka = "", model = "", rodzaj.paliwa = "", 
                         moc.silnika = 0, rok.produkcji= 0, przebieg= 0) {
   
   stopifnot(is.character(marka), is.character(model), 
             is.character(rodzaj.paliwa), is.numeric(moc.silnika),
             is.numeric(rok.produkcji), is.numeric(przebieg))
   stopifnot(length(marka) == 1, length(model) == 1, 
             length(rodzaj.paliwa) == 1, length(moc.silnika) == 1,
             length(rok.produkcji) == 1,length(przebieg) == 1)

   temp<-auta2012
   if (marka != "")
      temp <- temp %>% filter(Marka == marka)
   if (model != "")
      temp <- temp %>% filter(Model == model)
   if (rodzaj.paliwa != "")
      temp <- temp %>% filter(Rodzaj.paliwa == rodzaj.paliwa)
   if (moc.silnika > 0)
      temp <- temp %>% filter(KM >= moc.silnika)
   if (rok.produkcji > 0)
      temp <- temp %>% filter(Rok.produkcji == rok.produkcji)
   if (przebieg > 0)
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


