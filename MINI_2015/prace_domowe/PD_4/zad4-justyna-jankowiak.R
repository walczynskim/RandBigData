##### Zadanie domowe nr 4 #####
###### Justyna Jankowiak ######

#używam skróconych nazw:
#marka - Marka
#model - Model
#paliwo - Rodzaj.paliwa
#moc - KM
#rok - Rok.produkcji
#przebieg - Przebieg.w.km

statystyki <- function(marka="", model="", paliwo="", moc=-1, rok=-1, przebieg=-1){
   
   if(!all(is.character(marka), is.character(model), is.character(paliwo)))
      stop("Marka, model i rodzaj paliwa to napisy!")
   if(!all(is.numeric(moc), is.numeric(rok), is.numeric(przebieg)))
      stop("Moc silnika, rok produkcji oraz przebieg to zmienne numeryczne!")
   
   library(PogromcyDanych)
   library(dplyr)
   auta <- tbl_df(auta2012)
   
   argumenty <- c(marka, model, paliwo, 
                  as.character(moc), as.character(rok), as.character(przebieg))
   nazwy <- c("Marka", "Model", "Rodzaj.paliwa", "KM", "Rok.produkcji", "Przebieg.w.km")
   ktore.podane <- !(argumenty=="" | argumenty =="-1")
   
   for (i in seq_along(argumenty)){
      if (ktore.podane[i] == TRUE){
         target <- nazwy[i]
         call1 <- substitute(argumenty[i] %in% auta$target, list(target=as.name(target)))
         czy.jest.taka.wartosc <- eval(call1)
         if(czy.jest.taka.wartosc == FALSE){
            stop(paste("Nieistniejąca wartość dla", nazwy[i]))
         }
         
         call2 <- substitute(filter(auta, target==argumenty[i]), list(target=as.name(target)))
         auta <- eval(call2)
      }
   }
   
   stat <- auta %>% 
      summarise(miniumum = min(Cena.w.PLN, na.rm = TRUE),
                maksimum = max(Cena.w.PLN, na.rm = TRUE),
                kwantyl1 = quantile(Cena.w.PLN, 0.25, na.rm = TRUE),
                mediana = median(Cena.w.PLN, na.rm = TRUE),
                kwantyl3 = quantile(Cena.w.PLN, 0.75, na.rm = TRUE)
                )
   return(stat)
}

statystyki(marka = "Toyota", rok = 2008)
statystyki(marka = "Porsche", przebieg = 2) #nie ma takiego przebiegu
statystyki(model = "Golf", przebieg = 20000)
statystyki(model = "Golf", rok = 2005, przebieg = 20000) 
#petla idzie nam po kolei (ustalony porządek argumentów) wieć mimo że w 3. przykładzie taki przebieg dla Golfa istnieje
#to gdy dodamy rok (względem które filtrujemy przed przebiegiem) to dla Golfa z 2005 roku już takiego przebiegu nie ma
statystyki(marka = "Renault", paliwo = "benzyna", rok = 2000)
