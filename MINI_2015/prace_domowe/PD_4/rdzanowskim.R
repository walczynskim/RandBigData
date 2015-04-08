library("PogromcyDanych")
library("dplyr")
auta2012 <- tbl_df(auta2012)

summary2 <- function(marka = NULL, model = NULL, rodzaj_paliwa = NULL, moc_silnika = NULL, rok_produkcji = NULL, przebieg = NULL){
  cat("Wybrane statystyki dla aut spelniajacych nastepujace warunki: \n")
  i <- 0
  auta_wybor <- tbl_df(auta2012)
  if(! is.null(marka)){
    auta_wybor <- auta_wybor %>% filter(Marka == marka)
    i <- i+1
    cat("\t", paste(i, ".", sep = ""), " Marka =", marka, "\n")
  } 
  if(! is.null(model)){
    auta_wybor <- auta_wybor %>% filter(Model == model)
    i <- i+1
    cat("\t", paste(i, ".", sep = ""), " Model =", model, "\n")
  } 
  if(! is.null(rodzaj_paliwa)){
    auta_wybor <- auta_wybor %>% filter(Rodzaj.paliwa == rodzaj_paliwa)
    i <- i+1
    cat("\t", paste(i, ".", sep = ""), " Rodzaj.paliwa =", rodzaj_paliwa, "\n")
  } 
  if(! is.null(moc_silnika)){
    auta_wybor <- auta_wybor %>% filter(KM == moc_silnika) 
    i <- i+1
    cat("\t", paste(i, ".", sep = ""), " KM =", moc_silnika, "\n")
  } 
  if(! is.null(rok_produkcji)){
    auta_wybor <- auta_wybor %>% filter(Rok.produkcji == rok_produkcji) 
    i <- i+1
    cat("\t", paste(i, ".", sep = ""), " Rok.produkcji =", rok_produkcji, "\n")
  } 
  if(! is.null(przebieg)){
    auta_wybor <- auta_wybor %>% filter(Przebieg.w.km == przebieg)
    i <- i+1 
    cat("\t", paste(i, ".", sep = ""), " Przebieg.w.km =", przebieg, "\n")
  } 
  if(i == 0){
    cat("\t BRAK WARUNKOW! \n\n")
  }
  else{
    cat("\n")
  }
  
  # zwroc minimum, maksimum, kwartyle dla cen aut pasujacych do arg
  stat <- auta_wybor %>% summarise(Minimum = min(Cena.w.PLN), 
                                   Q1 = quantile(Cena.w.PLN, probs = 0.25), 
                                   Q2 = quantile(Cena.w.PLN, probs = 0.5,), 
                                   Q3 = quantile(Cena.w.PLN, probs = 0.75), 
                                   Maksimum = max(Cena.w.PLN))
  print(stat)
  return(invisible(stat))
}


summary2(marka = "Kia", model = "Carens", rodzaj_paliwa = "olej napedowy (diesel)", 
         moc_silnika = 140, rok_produkcji = 2008 , przebieg = 41000)


summary2(marka = "Kia", model = "Carens", rodzaj_paliwa = "olej napedowy (diesel)")


summary2(marka = "Kia", rodzaj_paliwa = "olej napedowy (diesel)")


summary2()
