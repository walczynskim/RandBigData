# skrypt był puszczany tylko na 3 urządzeniach, które analizowaliśmy: 19a, 38, 39

library(stringi)

sciezka<-getwd()

# lista folderóœ będących miesiącami
foldery <- list.files(file.path(sciezka,"projekt1/dane_csv"))

for (ff in foldery) {
   
   # wczytanie ramek danych
   dane_cale<-read.csv( file.path(sciezka,"projekt1/dane_csv",ff,"19a","/cnk19a_cale.csv") )
   dane<-read.csv( file.path(sciezka,"projekt1/dane_csv",ff,"19a","/cnk19a.csv") )
   
   n_dane<-nrow(dane)
   
   # wyznaczam czas odejscia od ekscponatu
   czas<-rep(0,n_dane)
   
   for (i in seq(1,n_dane)) {
      if (i==n_dane2){ # ostatni uzytkownik w danym miesiacu
         # szukamy ostatniego wiersza, w kotrym mamy zamknie systemu bardz blad lub cos innego
         tmp_dane<-as.vector(dane_cale$V6[(dane$nr[i]+1):nrow(dane_cale)]) 
         napis1<-stri_extract_first_regex(tmp_dane,"Shutting down") 
         if (all(is.na(napis1))) {
            napis2<-stri_extract_first_regex(tmp_dane,"WATCHDOG|ERR")
            nr<-min(which(!is.na(napis2))-1)
         } else {nr<- min(which(!is.na(napis1))-1)}
         czas[i]<-as.vector(dane_cale$V4[dane$nr[i]+nr])
      
      } else{
         if (dane$V3[i]==dane$V3[i+1]){ # ten sam miesiac dla dwoch kolejnych uzytkownikow
            
            if (dane$nr[i]+1==dane$nr[i+1]){ # uzytkownik nie wszedlw interakcje z eksponatem, 
               #np wlozyl i od razu wyjal karte 
               czas[i]<-as.vector(dane$V4[i])
            } else{ # byla interakcja uzytkownik - eksponat
               # wyznaczamy czas konca uzytkowania eksponatu, patrzac czy nie pojawily sie zardne 
               # komunikaty przed pojawieniem sie kolejnego uzytkownika
               tmp_tresc<-dane_cale$V6[dane$nr[i+1]-1]
               blad_1<-stri_extract_first_regex(tmp_tresc,"No visitor with")
               blad_2<-stri_extract_first_regex(tmp_tresc,"More than one visitor")
               
               if (!is.na(blad_1) | !is.na(blad_2)){
                  czas[i]<-as.vector(dane_cale$V4[dane$nr[i+1]-2])
               } else {
                  czas[i]<-as.vector(dane_cale$V4[dane$nr[i+1]-1])
               }
            }
         } else{ #rozny miesiac dla dwoch kolejnych uzytkownikow w ramce
            # wyszukujemy czasu poprzez wyznaczenie zamkniecia systemu w wierszach 
            # pomiedzy logowaniami uzytkownikow z roznych miesiecy
            tmp_dane<-as.vector(dane_cale$V6[(dane$nr[i]+1):(dane$nr[i+1]-1)])
            napis1<-stri_extract_first_regex(tmp_dane,"Shutting down") 
            if (all(is.na(napis1))) {
               napis2<-stri_extract_first_regex(tmp_dane,"WATCHDOG|ERR")
               nr<-min(which(!is.na(napis2))-1)
            } else {nr<- min(which(!is.na(napis1))-1)}
            czas[i]<-as.vector(dane_cale$V4[dane$nr[i]+nr])
         }
      }
   }
   
   
   
   # utworzenie kolumny z data w lepszym formacie
   datyy<-rep(0,n_dane)
   datyy<-stri_datetime_parse( stri_join(as.vector(dane$"V2"), as.vector(dane$"V3"),"2013",sep=" "),"date_long",
                               locale = "en_EN")
   datyy<-as.Date(datyy)
   
   # dodanie kolumny z dniem tygodnia
   dzien_tyg<-strftime(datyy,'%A')
   
   # utworzenie kolumny z liczba sekund spedzona przy urzadzeniu
   # na poczatek przeksztacimy date i czas tak abysmy mogli je odjac od siebie
   poczatek<-as.vector(dane$V4)
   poczatek_format<-paste(datyy,poczatek)
   koniec_format<-paste(datyy,czas)
   koniec_calosc<-strptime(koniec_format,"%Y-%m-%d %H:%M:%S")
   pocztek_calosc<-strptime(pocztek_format,"%Y-%m-%d %H:%M:%S")
   # wyliczamy czas spedzony przy urzadzeniu
   roznica_czasow<-difftime(koniec_calosc,poczatek_calosc,units = "secs")
   
   # tworzymy ramke danych z odpowiednimi kolumnami
   dane_ost<-cbind(nr=dane$"nr",wiersz=as.vector(dane$"V1"),miesiac=as.vector(dane$"V2"),
                   dzien=as.vector(dane$"V3"),data=as.character(datyy),dzien_tyg=dzien_tyg,rozpoczecie=as.vector(dane$"V4"),
                   zakonczenie=czas, roznica_czas=roznica_czasow,id=as.vector(dane$"V7"),sesja=as.vector(dane$"V8"),
                   urzadzenie=as.vector(dane$"V5"))
   
   # zapisanie powstalej ramki danych
   write.csv(dane_ost, file.path(sciezka,"projekt1/dane_csv",ff,"19a","/cnk19a_ost.csv") )
   
   # wypisanie folderu, w celu kontroli na jakim etapie jestem 
   print(ff)
}
