
Auta.statystyki<-function(marka="",model="",rodzaj.paliwa="",moc.silnika.w.km=0,
   rok.produkcji=0,przebieg.w.km=0,wyswietl=FALSE){

   library(PogromcyDanych)
   library(dplyr)
   statystyki<-tbl_df(auta2012)

   if(marka!=""){
      statystyki<-filter(statystyki,Marka==marka)
   }
   if(model!=""){
      statystyki<-filter(statystyki,Model==model)
   }
   if(rodzaj.paliwa!=""){
      statystyki<-filter(statystyki,Rodzaj.paliwa==rodzaj.paliwa)
   }
   if(moc.silnika.w.km>0){
      statystyki<-filter(statystyki,KM>moc.silnika.w.km)
   }
   if(rok.produkcji>0){
      statystyki<-filter(statystyki,Rok.produkcji==rok.produkcji)
   }
   if(przebieg.w.km>0){
      statystyki<-filter(statystyki,Przebieg.w.km<przebieg.w.km)
   }
   if(wyswietl==TRUE){ #jeśli ktoś by chciał wyświetlić otrzymaną ramkę
      print(statystyki)
   }

   statystyki<-statystyki %>%
      summarise(Min=min(Cena.w.PLN),
                Q1=quantile(Cena.w.PLN,0.25),
                Med=quantile(Cena.w.PLN,0.5),
                Q3=quantile(Cena.w.PLN,0.75),
                Max=max(Cena.w.PLN)
                )
   statystyki
}

Auta.statystyki()
Auta.statystyki("Honda")
Auta.statystyki("Honda","Civic")
Auta.statystyki("Honda","Civic","benzyna")
Auta.statystyki("Honda","Civic","benzyna",75)
Auta.statystyki("Honda","Civic","benzyna",75,1996,wyswietl=TRUE)
Auta.statystyki("Honda","Civic","benzyna",75,1996,300000,wyswietl=TRUE)
