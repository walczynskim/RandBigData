PodsumowanieAuta <-function(Marka="", Model="", Rodzaj.paliwa="", Moc.silnika=0, 
                          Rok.produkcji=0, Przebieg=0){
  
  stopifnot(is.character(Marka),is.character(Model),is.character(Rodzaj.paliwa),
            is.numeric(Moc.silnika),is.numeric(Rok.produkcji),
            is.numeric(Przebieg))
  
  library(PogromcyDanych)
  library(dplyr)
  auta2012 <- tbl_df(auta2012)
  
  if(Marka != ""){ auta2012 <- filter(auta2012, Marka == Marka) }
  if(Model != ""){ auta2012 <- filter(auta2012, Model == Model) }
  if(Rodzaj.paliwa != ""){ auta2012 <- filter(auta2012, Rodzaj.paliwa == Rodzaj.paliwa) }
  if(Moc.silnika != 0){ auta2012 <- filter(auta2012, KM == Moc.silnika) }
  if(Rok.produkcji != 0){ auta2012 <- filter(auta2012, Rok.produkcji == Rok.produkcji) }
  if(Przebieg != 0){ auta2012 <- filter(auta2012, Przebieg.w.km == Przebieg) }

    summarise(auta2012, Min=min(Cena.w.PLN), q1=quantile(Cena.w.PLN,0.25),
              Med = quantile(Cena.w.PLN,0.5), q3=quantile(Cena.w.PLN,0.75),
              Max=max(Cena.w.PLN))
}

PodsumowanieAuta("Kia", "Carens", "olej napedowy (diesel)", 140, 2008,41000)
PodsumowanieAuta("Kia", "Carens", "olej napedowy (diesel)", 140)
PodsumowanieAuta("Kia", "Carens", "olej napedowy (diesel)")
PodsumowanieAuta()
