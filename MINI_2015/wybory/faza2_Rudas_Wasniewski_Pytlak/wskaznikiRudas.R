#Slownik

kandydat <- c(
   "Komorowski\\p{L}*",
   "Marian(\\p{L})* Kowalsk(\\p{L})*",
   "(Dud(\\p{L})*)",
   "Paliko(\\p{L})*",
   "Jarubas(\\p{L})*",
   "Ogórek",
   "Korwin(\\p{L})*",
   "Wilk(\\p{L})*",
   "Braun(\\p{L})*",
   "Kukiz(\\p{L})*"
)
nazwiska <- c("komorowski","kowalski","duda","palikot","jarubas","ogórek","korwin",
              "wilk","braun","kukiz")
names(kandydat) <- nazwiska


sciezka<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\efkonc" #do folderu gdzie znajduja sie pobrane dane
#sciezka - sciezka dostepu
#podajzrodlo - z jakiego zrodla bierzemy artykuly (gazeta.pl,tvn24.pl,wiadomosci.wp.pl,wiadomosci.onet.pl)
#co - kolumna w ktorej badamy wystepowanie kandydata(tytul,tresc,tagi)
#datapocz - data początkowa, od ktorej zaczynamy analize artykulow
#datakonc - data koncowa, na ktorej konczymy analize artykulow
#funkcja - jaka miare czestotliwosci chcemy (np. mean, median, maks)
#po.zrodle - czy rozpatrujemy artykuly nie patrzac na zrodlo czy moze dla konkretnego zrodla
czestotliwosc<-function(sciezka,podajzrodlo,co,datapocz,datakonc,funkcja=mean,po.zrodle=FALSE)
{
   #Biblioteka
   library(stringi)
   
   #Przerobienie na POSIX
   datapocz<-stri_paste(datapocz,"00:00",sep=" ")
   datakonc<-stri_paste(datakonc,"23:59",sep=" ")
   pocz<-strptime(datapocz,"%d-%m-%Y %H:%M")
   konc<-strptime(datakonc,"%d-%m-%Y %H:%M")
   # Pobieramy dane pobrane przez nasze portale:
   
   #Wybieramy najbardziej aktualny plik
   
   lista<-list.files(sciezka)
   n<-length(lista)
   aktualny<-lista[n]
   aktualny
   
   #Sciezka dostepu do aktualnego
   
   sciezka<-stri_paste(sciezka,"\\")
   sciezka<-stri_paste(sciezka,aktualny)
   
   #Wczytajmy plik
   
   dane<-read.table(sciezka)
   
   #Chcemy rozpatrywac tylko z podanego zrodla, jesli po.zrodle=FALSE
   if(po.zrodle)
   {
      danegazeta<-dane[dane$zrodlo==podajzrodlo,]
   }
   else
   {
      danegazeta<-dane
   }
   # Ponizsza lista bedzie przechowywac roznice czasow miedzy kolejnymi artykulami
   czasykandydatow<-list()
   #wektor wag
   waga<-rep(0,length(kandydat))
   #Czas aktualny
   #wektor ktory bedzie zliczal maksymalna roznice czasu (miedzy datakonc i data pocz)
   maxtime<-as.numeric(konc-pocz,units="hours")
   #licz dla kazdego kandydata wektor opisujacy roznice w godzinach miedzy kolejnymi artykulami
   for(i in 1:length(kandydat))
   {
      #czasy artykulow kandydatow
      artykulykandydatow<-danegazeta[stri_detect_regex(danegazeta[,co],kandydat[i]),]$data
      #zamiana na POSIXct i sortowanie czasow
      
      artykulykandydatow <- stri_replace_all_regex(artykulykandydatow ,
                                                   "([0-9]{4})-([0-9]{2})-([0-9]{2})", 
                                                   "$3-$2-$1")
      times<-sort(strptime(artykulykandydatow, "%d-%m-%Y %H:%M"))
      
      # chcemy miec tylko te ktore sie mieszcza w przedziale [datapocz,datakonc]
      times<-times[which(times>=pocz&times<=konc)]
      
      
      # (zabezpieczamy sie przed sytuacja dlugo nic, 
      # dwa artykuly blisko siebie, dlugo nic temu samemu sluzy odejmowanie od czasu pierwszego
      # artykulu czasu pierwszego pobranego artykulu dla danego portalu)
      times1<-c(times,konc)
      times2<-c(pocz,times)
      #liczymy roznice
      czasykandydatow[[i]]<-as.numeric(times1-times2,units="hours")
   }
   #wylicz miare
   srednie<-sapply(czasykandydatow,funkcja)
   #przypisz nazwy
   names(srednie)<-nazwiska
   #Przyjmuje skale widocznosci od 0 do 5
   #0 - zaden artykul sie nie pojawil
   #1 - sredni czas powyzej 168h(tydzien)
   #2 - sredni czas ponizej 168h(tydzien)
   #3 - sredni czas ponizej 96h(cztery doby)
   #4 - sredni czas ponizej 48h(dwie doby)
   #5 - sredni czas ponizej 24h(doba)
   
   waga[which(srednie<24)]<-5
   waga[which(srednie>=24&srednie<48)]<-4
   waga[which(srednie>=48&srednie<96)]<-3
   waga[which(srednie>=96&srednie<168)]<-2
   waga[which(srednie>=168&srednie<maxtime)]<-1
   names(waga)<-nazwiska
   list(srednie,waga)
}
#przykladowe wywolania
czestotliwosc(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)
czestotliwosc(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),"07-04-2015",po.zrodle=TRUE)
czestotliwosc(sciezka,NULL,"tytul",datapierwszgopobrania(sciezka,NULL,FALSE),aktualnydzien(),po.zrodle=FALSE)
czestotliwosc(sciezka,"tvn24.pl","tresc",datapierwszgopobrania(sciezka,"tvn24.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)
czestotliwosc(sciezka,"wiadomosci.wp.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.wp.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)
czestotliwosc(sciezka,"wiadomosci.onet.pl","tagi",datapierwszgopobrania(sciezka,"wiadomosci.onet.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)
czestotliwosc(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),funkcja=median,po.zrodle=TRUE)

#Funkcja ma na celu wyciagniecie dla kazdego kandydata najdluzszego ciagu dni,
#gdzie nieprzerwanie kazdego dnia pojawial sie nowy artykul dotyczacy kandydata (dlamks=TRUE).
#Dla maks=FALSE zlicza ile bylo ciagow nastepujacych po sobie dni >=dlserii kazdego dla kandydata
#Reszta argumentow jak w analiza czestotliwosci
iloscDni<-function(sciezka,podajzrodlo,co,datapocz,datakonc,maks=TRUE,dlserii=2,po.zrodle=FALSE)
{
   #Biblioteka
   library(stringi)
   
   #Przerobienie na POSIX
   pocz<-strptime(datapocz,"%d-%m-%Y")
   konc<-strptime(datakonc,"%d-%m-%Y")
   # Pobieramy dane pobrane przez nasze portale:
   #Wybieramy najbardziej aktualny plik
   
   lista<-list.files(sciezka)
   n<-length(lista)
   aktualny<-lista[n]
   
   #Sciezka dostepu do aktualnego
   
   sciezka<-stri_paste(sciezka,"\\")
   sciezka<-stri_paste(sciezka,aktualny)
   
   #Wczytajmy plik
   dane<-read.table(sciezka)
   
   #Jesli po.zrodle==TRUE, to chcemy rozpatrywac tylko z podanego zrodla
   if(po.zrodle)
   {
      danegazeta<-dane[dane$zrodlo==podajzrodlo,]
   }
   else
   {
      danegazeta<-dane
   }
   #wektor zliczajacy dlugosc maksymalnej serii dla danego kandydata
   maksymalnadlugosc<-rep(0,length(kandydat))
   names(maksymalnadlugosc)<-nazwiska
   #wektor zliczajacy liczbe seri o dlugosci niemniejszej niz dlserii
   liczba<-rep(0,length(kandydat))
   names(liczba)<-nazwiska
   for(i in 1:length(kandydat))
   {
      #czasy artykulow kandydatow
      artykulykandydatow<-danegazeta[stri_detect_regex(danegazeta[,co],kandydat[i]),]$data
      artykulykandydatow <- stri_replace_all_regex(artykulykandydatow ,
                                                   "([0-9]{4})-([0-9]{2})-([0-9]{2})", 
                                                   "$3-$2-$1")
      
      times<-unique(sort(strptime(artykulykandydatow, "%d-%m-%Y")))
      #chcemy miec tylko z zadanego przedzialu czasowego   
      times<-times[which(times>=pocz&times<=konc)]
      
      if (length(times)!=0)
      {
         #jesli mamy dlugosc times rowna 1 to mamy tylko serie dlugosci 1
         if(length(times)==1)
         {
            maksymalnadlugosc[i]<-1
            if(dlserii==1)
            {
               liczba[i]<-1 
            }
            else
            {
               liczba[i]<-0
            }
            
         }
         else
         {
            times1<-times[-1]
            times2<-times[-length(times)]
            iledni<-ceiling(as.numeric(times1-times2,unit="days"))
            
            #iterator petli
            j<-1
            #zmienna pomocnicza zliczajaca aktualna dlugosc serii
            pom<-0
            #zmienna pomocnicza zliczajaca dlugosci serii dla kandydata
            wektorilosci<-c()
            while(j<=length(iledni))
            {
               if(iledni[j]==1)
               {
                  pom<-pom+1
                  if(j==length(iledni))
                  {
                     wektorilosci<-c(wektorilosci,pom)
                  }
               }
               else
               {
                  wektorilosci<-c(wektorilosci,pom)
                  pom<-0
               }
               if(pom+1>maksymalnadlugosc[i])
               {
                  maksymalnadlugosc[i]<-pom+1
               }
               j<-j+1
               
            }
            wektorilosci<-wektorilosci+1
            #wybierz tylko te ktore maja wartosc nie mniejsza niz dlserii
            liczba[i]<-length(wektorilosci[wektorilosci>=dlserii])
            
            
         }
      }
      
      
   }
   if(maks)
   {
      maksymalnadlugosc
   }
   else
   {
      liczba
   }
   
}
#przykladowe wywolania
iloscDni(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)
iloscDni(sciezka,NULL,"tytul",datapierwszgopobrania(sciezka,NULL,FALSE),aktualnydzien())
iloscDni(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),maks=FALSE,5,po.zrodle=TRUE)
iloscDni(sciezka,"wiadomosci.wp.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.wp.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)
iloscDni(sciezka,"wiadomosci.onet.pl","tagi",datapierwszgopobrania(sciezka,"wiadomosci.onet.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)
iloscDni(sciezka,"tvn24.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.onet.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)

#Funkcja przyjmuje pewien przedzial czasowy i dla kazdego kandydata zwraca nastepujacy wskaznik
#ilosc dni w ktorym pojawial sie artykul o kandydacie w rozpatrywanym okresie czasu
#argumenty jak wyzej
#uprzejmie prosi sie o podawanie arg datapocz i datakonc w postaci dd-mm-rrrr
przezileDni<-function(sciezka,podajzrodlo,co,datapocz, datakonc,po.zrodle=FALSE)
{
   #Biblioteki
   library(stringi)
   library(dplyr)
   library(tidyr)
   #Zamina na POSIX
   pocz<-strptime(datapocz,"%d-%m-%Y")
   konc<-strptime(datakonc,"%d-%m-%Y")
   #Ile dni od datapocz do datakonc
   liczbadni<-ceiling(as.numeric(konc-pocz,units="days"))+1
   #ramka danych o czestotliwosci wystepowan ze wzgledu na dzien
   d<-ile.dziennie(sciezka,co,po.zrodle)
   if(po.zrodle)
   {
      d<-d[d$zrodlo==podajzrodlo,]
      odpowiedniekolumny<-2 #kolumny kandydatow zaczynaja sie od 3
   }
   else
   {
      odpowiedniekolumny<-1 #kolumny kandydatow zaczynaja sie od 2  
   }
   #wektor miary
   miara<-rep(0,length(kandydat))
   #dla kazdego kandydata zliczaj dni w ktorych jest w artykule i te ktore <=datakonc i >=datapocz
   for(i in 1:length(kandydat))
   {
      dni<-strptime(d[which(d[,(odpowiedniekolumny+i)]>0),]$dzien,"%d-%m-%Y")
      miara[i]<-length(which(dni<=konc&dni>=pocz))/liczbadni
   }
   names(miara)<-nazwiska
   miara
}
#przykladowe wywolania
przezileDni(sciezka,"gazeta.pl","tresc",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),TRUE)
przezileDni(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),TRUE)
przezileDni(sciezka,NULL,"tytul",datapierwszgopobrania(sciezka,NULL,FALSE),aktualnydzien(),FALSE)
przezileDni(sciezka,"tvn24.pl","tytul",datapierwszgopobrania(sciezka,"tvn24.pl",TRUE),aktualnydzien(),TRUE)

#Zliczanie liczby wystapien nazwiska kandydata w danym okresie czasu ze wzgledu na zrodlo (lub po calosci)
ilezliczenprzezDni<-function(sciezka,podajzrodlo,co,datapocz, datakonc,po.zrodle=FALSE)
{
   #Biblioteki
   library(stringi)
   library(dplyr)
   library(tidyr)
   #Zamina na POSIX
   pocz<-strptime(datapocz,"%d-%m-%Y")
   konc<-strptime(datakonc,"%d-%m-%Y")
   #ramka danych o czestotliwosci wystepowan ze wzgledu na dzien
   d<-ile.dziennie(sciezka,co,po.zrodle)
   if(po.zrodle)
   {
      d<-d[d$zrodlo==podajzrodlo,]
      odpowiedniekolumny<-2 #kolumny kandydatow zaczynaja sie od 3
   }
   else
   {
      odpowiedniekolumny<-1 #kolumny kandydatow zaczynaja sie od 2  
   }
   zliczaktywnosc<-rep(0,length(kandydat))
   dni<-strptime(d$dzien,"%d-%m-%Y")
   #ktore dni sie mieszcza w przedziale
   d<-d[which(dni<=konc&dni>=pocz),]
   #sumujemy aktywnosc
   for(i in 1:length(kandydat))
   {
      zliczaktywnosc[i]<-sum(d[,odpowiedniekolumny+i])
   }
   names(zliczaktywnosc)<-nazwiska
   zliczaktywnosc
   
}
#przykladowe iwywolania
ilezliczenprzezDni(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),TRUE)
ilezliczenprzezDni(sciezka,"wiadomosci.wp.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.wp.pl",TRUE),aktualnydzien(),TRUE)

# Funkcja ma na celu wybadanie ilości wystąpień danego kandydata (tytuły atrykułów, tagi, treści)
# ze względu na dni powszednie i na weekendy
# podajzrodlo - zrodlo z ktorego bylo pobierane (gazeta.pl,wiadomosci.wp.pl,wiadomosci.onet.pl,tvn24.pl)
# co - kolumna którą poddajemy analizie (tytul,tresc,tagi)
# datapocz - poczatkowa data, od ktorej zaczynamy liczyc artykuly
# datakonc - koncowa data liczenia artykulow
# po.zrodle - czy w ogole patrzymy tylko na jedne zrodlo, czy tez na calosc zebranych danych, FALSE oznacza, ze po calosci
analizaweek<-function(sciezka,podajzrodlo,co,datapocz,datakonc,po.zrodle=FALSE)
{
   #Biblioteki
   library(stringi)
   library(dplyr)
   library(tidyr)
   #Przerobienie na POSIX
   pocz<-strptime(datapocz,"%d-%m-%Y")
   konc<-strptime(datakonc,"%d-%m-%Y")
   
   # Wywoluje funkcje, ktora liczy dla konkretnych dni ilosc wystapien kandydatow
   d<-ile.dziennie(sciezka,co,po.zrodle)
   # Jesli rozpatrujemy konkretne zrodlo to zawezmy ramke danych tylko do tego zrodla 
   if(po.zrodle)
   {
      d<-d[d$zrodlo==podajzrodlo,]
      odpowiedniekolumny<-2 #kolumny kandydatow zaczynaja sie od 3
   }
   else
   {
      odpowiedniekolumny<-1 #kolumny kandydatow zaczynaja sie od 2  
   }
   #Zamiana na POSIX
   dni<-strptime(d$dzien,"%d-%m-%Y")
   # Rozpatrujemy tylko te artykuly, ktore mieszcza sie w przedziale 
   # wyznaczonym przez parametry funkcji datapocz i datakonc
   d<-d[which(dni<=konc&dni>=pocz),]
   #zmieniamy format z dd-mm-rrrr na rrrr-mm-dd
   d$dzien<-stri_replace_all_regex(d$dzien,
                                   "([0-9]{2})-([0-9]{2})-([0-9]{4})",
                                   "$3-$2-$1")
   # wyciagamy z dat informacje o tym jaki dzien tygodnia wtedy byl
   dnitygodnia<-as.POSIXlt(d$dzien)$wday
   # ograniczamy sie tylko do dni weeknedowych
   dweek<-d[which(dnitygodnia==0|dnitygodnia==6),]
   # ograniczamy sie tylko do dni powszednich
   dpowsz<-d[which(dnitygodnia!=0&dnitygodnia!=6),]
   zliczaktywnoscweek<-rep(0,length(kandydat))
   zliczaktywnoscpowsz<-rep(0,length(kandydat))
   for(i in 1:length(kandydat))
   {
      zliczaktywnoscweek[i]<-sum(dweek[,odpowiedniekolumny+i])
      zliczaktywnoscpowsz[i]<-sum(dpowsz[,odpowiedniekolumny+i])
   }
   names(zliczaktywnoscweek)<-nazwiska
   names(zliczaktywnoscpowsz)<-nazwiska
   l<-list(zliczaktywnoscweek,zliczaktywnoscpowsz)
   names(l)=c("weekendowe","powszednie")
   l
}
#przykladowe wywolania
analizaweek(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",FALSE),aktualnydzien(),TRUE)
analizaweek(sciezka,"tvn24.pl","tytul",datapierwszgopobrania(sciezka,"tvn24.pl",TRUE),aktualnydzien(),TRUE)
analizaweek(sciezka,"wiadomosci.onet.pl","tagi",datapierwszgopobrania(sciezka,"wiadomosci.onet.pl",TRUE),aktualnydzien(),TRUE)
analizaweek(sciezka,"wiadomosci.wp.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.wp.pl",TRUE),aktualnydzien(),TRUE)

