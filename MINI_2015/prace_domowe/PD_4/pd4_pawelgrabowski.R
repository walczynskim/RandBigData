SelektorAut <- function(model = NA_character_, marka = NA_character_,
                      rodzajPaliwa = NA_character_, mocSilnika = NA_real_,
                      rokProdukcji = NA_real_, przebieg = NA_real_) {
  
  stopifnot(all(sapply(list(model, marka, rodzajPaliwa), 
                   FUN = function(x) is.character(x))))
  stopifnot(all(sapply(list(mocSilnika, rokProdukcji, przebieg),
                   FUN = function(x) is.numeric(x))))
  stopifnot(all(sapply(list(model, marka, rodzajPaliwa, mocSilnika, rokProdukcji, 
                     przebieg), FUN = function(x) length(x) == 1)))
  
  require(PogromcyDanych)
  require(dplyr)
  
  auta <- tbl_df(auta2012)
  
  if (!is.na(model)) {
    auta <- filter(auta, Model == model)
  }
  if (!is.na(marka)) {
    auta <- filter(auta, Marka == marka)
  }
  if (!is.na(rodzajPaliwa)) {
    auta <- filter(auta, Rodzaj.paliwa == rodzajPaliwa)
  }
  if (!is.na(mocSilnika)) {
    auta <- filter(auta, KM >= mocSilnika)
  }
  if (!is.na(rokProdukcji)) {
    auta <- filter(auta, Rok.produkcji >= rokProdukcji)
  }
  if (!is.na(przebieg)) {
    auta <- filter(auta, Przebieg.w.km <= przebieg)
  }
  
  if( nrow(auta) > 0) {
    result <- summarise(auta, min = min(Cena.w.PLN, na.rm = TRUE),
                      max = max(Cena.w.PLN, na.rm = TRUE),
                      q1 = quantile(Cena.w.PLN, 0.25),
                      q2 = quantile(Cena.w.PLN, 0.5),
                      q3 = quantile(Cena.w.PLN, 0.75))
    return(result)
  } else {
    warning("Brak danych do analizy")
    return(NULL)
  }
  
}

SelektorAut(model="Sprinter", przebieg=100000, rokProdukcji=2000)
# > SelektorAut(model="Sprinter", przebieg=100000, rokProdukcji=2000)
# Source: local data frame [1 x 5]
# 
# min    max    q1    q2    q3
# 1 11000 204000 25450 62350 81175

SelektorAut(model="Sprinter", marka="Volvo")
# > SelektorAut(model="Sprinter", marka="Volvo")
# NULL
# Warning message:
#   In SelektorAut(model = "Sprinter", marka = "Volvo") :
#   Brak danych do analizy

SelektorAut(przebieg="asdasd")
#error
SelektorAut(marka=123)
#error
SelektorAut(rodzajPaliwa=c("gaz","benzyna"))
#error
