library(dplyr)
library(PogromcyDanych)

tmp <- tbl_df(auta2012)
info=function(marka,model,rodzaj_paliwa,moc_silnika,rok_produkcji,przebieg){
if (!missing(marka)) {tmp <- filter(tmp,
                                   Marka == marka) }
if (!missing(model)) {tmp <- filter(tmp,
                                    Model == model)}
if (!missing(rodzaj_paliwa)) {tmp <- filter(tmp,
                                            Rodzaj.paliwa == rodzaj_paliwa)}
if (!missing(moc_silnika)) {tmp <- filter(tmp,
                                          KM == moc_silnika)}
if (!missing(rok_produkcji)) {tmp <- filter(tmp,
                                            Rok.produkcji == rok_produkcji)}
if (!missing(przebieg)) {tmp <- filter(tmp,
                                       Przebieg.w.km == przebieg)}
tmp %>%
  summarise(minimum = min(Cena.w.PLN, na.rm=TRUE),
            maksimum = max(Cena.w.PLN, na.rm=TRUE),
            Q1=quantile(Cena.w.PLN, probs=0.25), 
            Q2=quantile(Cena.w.PLN, probs=0.50), 
            Q3=quantile(Cena.w.PLN, probs=0.75)
            )
}
#przyklady:
info(model='Carens',marka='Kia')
info(marka='Kia')
info()
