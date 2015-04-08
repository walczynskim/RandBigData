wybierz<-function(marka="", model="", rodzaj_pal="", moc=-1, rok=-1,przebieg=-1){
  
  library(PogromcyDanych)
  library(dplyr)
  
  nowy<-tbl_df(auta2012)
  argumenty <- list(list("Marka",c(marka,"")),list("Model",c(model,"")),
                    list("Rodzaj.paliwa",c(rodzaj_pal,"")),
                    list("KM",c(moc,-1)),list("Rok.produkcji",c(rok,-1)),
                    list("Przebieg.w.km",c(przebieg,-1)))

    for(i in 1:6){
      if(argumenty[[i]][[2]][[1]]!=argumenty[[i]][[2]][[2]]){
       nowy <- nowy %>%
          filter(nowy[,argumenty[[i]][[1]]]==argumenty[[i]][[2]][[1]])    
      }
    }
  
    if(nrow(nowy)>0){
    wyniki<-c(minimum=min(nowy$Cena.w.PLN,na.rm = TRUE), maximum=max(nowy$Cena.w.PLN,na.rm=TRUE),
             mediana=median(nowy$Cena.w.PLN,na.rm = TRUE),
             kwantyl=quantile(nowy$Cena.w.PLN,0.25,na.rm = TRUE),kwantyl=quantile(nowy$Cena.w.PLN,0.75,na.rm = TRUE))
  }else wyniki<-c(min=NA,max=NA,mediana=NA,"kwantyl25%"=NA,"kwantyl75%"=NA)
  
  return(wyniki)
}

wybierz("BMW")
wybierz("BMW","530",moc=245)
wybierz("BMW","530","benzyna",moc=245)
wybierz()
