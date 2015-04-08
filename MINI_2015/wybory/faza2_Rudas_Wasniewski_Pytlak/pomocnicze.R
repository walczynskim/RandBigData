#Zwraca aktualny dzien jako napis w postaci dd-mm-rrrr
aktualnydzien<-function()
{
   #Biblioteka
   library(stringi)
   t<-unlist(stri_extract_all_regex(strftime(Sys.time()),
                                 "([0-9]{4})-([0-9]{2})-([0-9]{2})"))
   stri_replace_all_regex(t,"([0-9]{4})-([0-9]{2})-([0-9]{2})",
                             "$3-$2-$1")
}

#Funkcja zwraca date pierwszego pobrania
#sciezka - sciezka dostepu do folderu gdzie znajduja sie pliki
#podajzrodlo - ktore zrodlo rozpatrujemy
#po.zrodlach - parametr logiczny, ktory mowi czy szukamy daty pobrania pierwszego artykulu
#dla zrodla czy tez ogolem
sciezka<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\efkonc"
datapierwszgopobrania<-function(sciezka,podajzrodlo,po.zrodlach=FALSE)
{
   #Biblioteka
   library(stringi)
   #Wybieramy najbardziej aktualny plik
   
   lista<-list.files(sciezka)
   n<-length(lista)
   aktualny<-lista[n]
   
   # Sciezka dostepu do aktualnego
   
   sciezka<-stri_paste(sciezka,"\\")
   sciezka<-stri_paste(sciezka,aktualny)
   #Wczytywanie danych
   d<-read.table(sciezka)
   #jesli szukamy po zrodle to rozpatrujemy tylko artykuly z danego zrodla
   if(po.zrodlach)
   {
      d<-d[d$zrodlo==podajzrodlo,]
   }
   daty<-d$data
   #Chce, aby wsztstkie daty mialy taka sama postac.
   daty <- stri_replace_all_regex(daty,
                                      "([0-9]{4})-([0-9]{2})-([0-9]{2})", 
                                      "$3-$2-$1")
      times<-sort(strptime(daty,"%d-%m-%Y"))
   #znajdujemy date pierwszego wywolania
   pierwszewywolanie<-times[1]
   #przerabiamy na napis
   strftime(pierwszewywolanie,"%d-%m-%Y")
}