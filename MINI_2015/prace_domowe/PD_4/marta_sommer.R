# R and Big Data - praca domowa nr 4
# Marta Sommer

# biblioteki:

library("stringi")
library("PogromcyDanych")
library("dplyr")

# dane:

auta <- tbl_df(auta2012)

# funkcja:

wybierz_samochody <- function(
                        marka = NULL, 
                        model = NULL, 
                        rodzaj_paliwa = NULL,
                        min_moc_silnika = NULL,
                        max_moc_silnika = NULL,
                        min_rok_produkcji = NULL,
                        max_rok_produkcji = NULL,
                        przebieg_ponizej= NULL){
  
  czy_null <- !c(is.null(marka), is.null(model), is.null(rodzaj_paliwa), 
                 is.null(min_moc_silnika), is.null(max_moc_silnika), 
                 is.null(min_rok_produkcji), is.null(max_rok_produkcji),
                 is.null(przebieg_ponizej))
  
  if(sum(czy_null) == 0) return(quantile(auta$Cena.w.PLN, na.rm=TRUE))
  
  argumenty <- c("marka", "model", "rodzaj_paliwa", 
                 "min_moc_silnika", "max_moc_silnika", 
                 "min_rok_produkcji", "max_rok_produkcji", 
                 "przebieg_ponizej")
  
  zmienne <- c("Marka", "Model", "Rodzaj.paliwa", 
               rep("kW", 2), rep("Rok.produkcji", 2), 
               "Przebieg.w.km")
  
  znaki <- c(rep(" %in% ", 3), rep(c(" >= ", " <= "), times=2), " <= ")
  
  aktualne_argumenty <- argumenty[czy_null]
  aktualne_zmienne <- zmienne[czy_null]
  aktualne_znaki <- znaki[czy_null]
  
  filtry <- character(length(aktualne_zmienne))
  
  for(i in 1:length(aktualne_zmienne)){
    filtry[i] <- stri_paste(aktualne_zmienne[i], 
                            aktualne_argumenty[i],
                            sep=aktualne_znaki[i])
  }
  
  argumenty_do_filter <- stri_paste(filtry, collapse=", ")
  
  cialo_funkcji <- stri_paste("auta %>% filter(", argumenty_do_filter, ")", 
                              " %>% \"$\"(\"Cena.w.PLN\") %>% quantile(na.rm=TRUE)", collapse="")
  
  eval(parse(text=cialo_funkcji))
  
}

# przyklady:

przykl1 <- wybierz_samochody(
  marka="Kia", 
  model="Carens", 
  rodzaj_paliwa="olej napedowy (diesel)", 
  min_moc_silnika=103,
  min_rok_produkcji=2008, 
  przebieg_ponizej=50000)

przykl2 <- wybierz_samochody(
  marka="Kia", 
  rodzaj_paliwa="olej napedowy (diesel)", 
  min_moc_silnika=103, 
  min_rok_produkcji=2008, 
  przebieg_ponizej=50000)

przykl3 <- wybierz_samochody(marka="Kia", przebieg_ponizej = 2000)

przykl4 <- wybierz_samochody()

przykl5 <- wybierz_samochody(marka="Toyota", model="coz_za_model!")
