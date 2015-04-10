
ceny <- function(marka = "", model = "", paliwo = "", moc = -1, rok = -1, 
                 przebieg = -1){
   
   stopifnot(is.character(marka), is.character(model), is.character(paliwo), 
             is.numeric(moc), is.numeric(rok), is.numeric(przebieg))
   
   library(PogromcyDanych)
   library(dplyr)

   auta2012 <- tbl_df(auta2012)
   
   marki <- auta2012 %>% select(Marka) %>% distinct()
   modele <- auta2012 %>% select(Model) %>% distinct()
   paliwa <- auta2012 %>% select(Rodzaj.paliwa) %>% distinct()
   moce <- auta2012 %>% select(KM) %>% distinct()
   lata <- auta2012 %>% select(Rok.produkcji) %>% distinct()
   przebiegi <- auta2012 %>% select(Przebieg.w.km) %>% distinct()
   
   filtr <- auta2012
   
   if(marka %in% marki$Marka){ 
      filtr <- filtr %>% filter(Marka == marka)
   }
   if(model %in% modele$Model){ 
      filtr <- filtr %>% filter(Model == model)
   }
   if(paliwo %in% paliwa$Rodzaj.paliwa){
      filtr <- filtr %>% filter(Rodzaj.paliwa == paliwo)
   }
   if(moc %in% moce$KM){
      filtr <- filtr %>% filter(KM == moc)
   }
   if(rok %in% lata$Rok.produkcji){
      filtr <- filtr %>% filter(Rok.produkcji == rok)
   }   
   if(przebieg %in% przebiegi$Przebieg.w.km){
      filtr <- filtr %>% filter(Przebieg.w.km == przebieg)
   }
   
   summarise(filtr, minimum = min(Cena.w.PLN, na.rm = TRUE), 
             pierwszyKwartyl = quantile(Cena.w.PLN, 0.25, na.rm = TRUE),
             mediana = median(Cena.w.PLN, na.rm = TRUE), 
             trzeciKwartyl = quantile(Cena.w.PLN, 0.75, na.rm = TRUE),
             maksimum = max(Cena.w.PLN, na.rm = TRUE))
}
 
#########################################################################   

ceny(marka = "Lexus", model = "IS 250", rok = 2006, przebieg = 80000)
ceny(marka = "Lexus", model = "IS 250", rok = 2007)

ceny(marka = "Honda", model = "Accord", paliwo = "benzyna", przebieg = 50000)
ceny(marka = "Honda", model = "Accord", paliwo = "benzyna", moc = 155, rok = 2004)

ceny(marka = "Nissan", model = "Primera", paliwo = "benzyna")
ceny(marka = "Nissan", model = "Primera", paliwo = "benzyna", moc = 130, rok = 1998)


