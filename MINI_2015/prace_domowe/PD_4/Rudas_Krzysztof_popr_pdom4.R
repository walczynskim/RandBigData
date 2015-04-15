statystykiauta<-function(marka=character(),model=character(),rodzajpaliwa=character(),
                     mocsilnika=integer(),rokprodukcji=integer(),przebieg=integer())
{
   stopifnot(is.character(marka),is.character(model),is.character(rodzajpaliwa),
             is.numeric(mocsilnika),is.numeric(rokprodukcji),is.numeric(przebieg))
   library(PogromcyDanych)
   library(dplyr)
   lista<-list(marka,model,rodzajpaliwa,mocsilnika,rokprodukcji,przebieg)
   #Zabezpieczenie przed zduplikowaniem wartosci w wektorach parametrow
   lista<-lapply(lista,unique)
   dlugosci<-sapply(lista,length)
   dane<-tbl_df(auta2012)
   naglowki<-c(which(names(dane)=="Marka"),which(names(dane)=="Model"),which(names(dane)=="Rodzaj.paliwa"),
               which(names(dane)=="KM"),which(names(dane)=="Rok.produkcji"),which(names(dane)=="Przebieg.w.km"))                                                         
   for(i in 1:length(dlugosci))
   {
      if(dlugosci[i]!=0)
      {
         dfpom<-data.frame()
         #filtrujmy element po elemencie i zapisujmy to do pomocniczej ramki
         for(j in 1:dlugosci[i])
         {
            d<-filter(dane,dane[naglowki[i]] == lista[[i]][j])
            #Nie musze sie martwic o unikaty,bo za kazdym razem filtrujemy po innej wartosci
            dfpom<-rbind(dfpom,d)
         }
         if(nrow(dfpom)==0)
         {
            stop("zle wartosci argumentow")
         }
         dane<-dfpom
      }
   }

   dane%>%summarise(minimum=min(Cena.w.PLN), pierwszykwantyl=quantile(Cena.w.PLN,0.25),
             mediana = quantile(Cena.w.PLN,0.5), trzecikwantyl=quantile(Cena.w.PLN,0.75),
             maksimum=max(Cena.w.PLN))
}
#testy
statystykiauta(marka = c("Volkswagen","Opel"), rok = 2006,model=c("Golf","Transporter","Astra"))
statystykiauta(marka = c("Opel","Opel"), rok = 2006,model=c("Golf","Transporter","Astra"))
statystykiauta(marka = c("Volkswagen","Volvo"), rok = 2006,model=c("Astra","Golf"))
statystykiauta(marka = "Volkswagen",mocsilnika = 0) #zla wartosc mocy silnika
statystykiauta(marka = "Fiat", rok = 2010,model="Punto")
statystykiauta(marka = "Fiat", rok = 2010,model="Punto",mocsilnika =c(75,65))

