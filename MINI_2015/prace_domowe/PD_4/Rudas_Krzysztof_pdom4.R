statystykiauta<-function(marka=character(),model=character(),rodzajpaliwa=character(),
                     mocsilnika=integer(),rokprodukcji=integer(),przebieg=integer())
{
   stopifnot(is.character(marka),is.character(model),is.character(rodzajpaliwa),
             is.numeric(mocsilnika),is.numeric(rokprodukcji),is.numeric(przebieg))
   library(PogromcyDanych)
   library(dplyr)
   lista<-list(marka,model,rodzajpaliwa,mocsilnika,rokprodukcji,przebieg)
   dlugosci<-sapply(lista,length)
   dane<-tbl_df(auta2012)
   naglowki<-c(which(names(dane)=="Marka"),which(names(dane)=="Model"),which(names(dane)=="Rodzaj.paliwa"),
               which(names(dane)=="KM"),which(names(dane)=="Rok.produkcji"),which(names(dane)=="Przebieg.w.km"))                                                         
   for(i in 1:length(dlugosci))
   {
      if(dlugosci[i]!=0)
      {
         dane<-filter(dane,dane[naglowki[i]] == lista[[i]])
         if(nrow(dane)==0)
         {
            stop("zle wartosci argumentow")
         }
      }
   }

   dane%>%summarise(minimum=min(Cena.w.PLN), pierwszykwantyl=quantile(Cena.w.PLN,0.25),
             mediana = quantile(Cena.w.PLN,0.5), trzecikwantyl=quantile(Cena.w.PLN,0.75),
             maksimum=max(Cena.w.PLN))
}
#testy
statystykiauta(marka = "Volkswagen", rok = 2006,model="Golf")
statystykiauta(marka = "Volkswagen",mocsilnika = 0) #zla wartosc mocy silnika
statystykiauta(marka = "Fiat", rok = 2010,model="Punto")
statystykiauta(marka = "Fiat", rok = 2010,model="Punto",mocsilnika =75)