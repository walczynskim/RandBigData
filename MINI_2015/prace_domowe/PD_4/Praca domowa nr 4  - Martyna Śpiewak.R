###################################################
### Praca domowa 4
### Martyna Œpiewak
###################################################

auta <- function(marka = character(), model = character(), rodzajPaliwa = character(), 
                 mocSilnika = numeric(), rokProdukcji = numeric(), przebieg = numeric()){
  require(dplyr)
  require(PogromcyDanych)
    
  auta2012 <- tbl_df(auta2012)
  
  if(length(marka) == 1) filter(auta2012, Marka == marka) -> tmp else tmp <- auta2012
  if(length(model) == 1) filter(tmp, Model == model) -> tmp
  if(length(rodzajPaliwa) == 1) filter(tmp, Rodzaj.paliwa == rodzajPaliwa) -> tmp
  if(length(mocSilnika) == 1) filter(tmp, KM > mocSilnika) -> tmp
  if(length(rokProdukcji) == 1) filter(tmp, Rok.produkcji > rokProdukcji) -> tmp
  if(length(przebieg) == 1) filter(tmp, Przebieg.w.km < przebieg) -> tmp
  
  res <- quantile(tmp$Cena.w.PLN)
  names(res)[c(1,5)] <- c("min", "max")
  
  return(res)
}

auta("Toyota", "Avensis", "benzyna", 120 , 2000, 20000)
# min      25%      50%      75%      max 
# 53468.75 56962.10 73000.00 83900.00 97900.00

auta(model = "Avensis", rodzajPaliwa = "benzyna", mocSilnika = 120, 
     rokProdukcji = 2010, przebieg = 30000)
# min      25%      50%      75%      max 
# 53468.75 56962.10 56962.10 79400.00 97900.00

auta()
# min        25%        50%        75%        max 
# 400.0    10900.0    19900.0    37470.9 11111111.0 
