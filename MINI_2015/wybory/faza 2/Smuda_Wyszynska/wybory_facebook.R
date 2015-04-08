library("Rfacebook")
library("devtools")
library("stringi")

##KRÓTKA INSTRUKCJA CO ZROBIĆ PRZED ODPALENIEM SKRYPTU

#1. Na początku trzeba stworzyć aplikację API na facebooku
#2. Potem tworzy się jakąś zmienną środowiskową np. fb_oauth
#  przy pomocy funkcji fbOAuth(app_id,app_secret,extended_permissions=TRUE)
#3. Następnie zapisuje się ją do pliku przy pomocy funkcji save(fb_oauth,file)
#4. Teraz można wczytać plik by zautomatyzować kod (pomijamy zewnętrzną autoryzację)

# By włączyć skrypt wciskamy CTRL+SHIFT+S

#wczytujemy plik z autoryzacją - trzeba zmienić tylko tu ścieżkę
load("D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/fb_oauth")

#ścieżka dla folderu z projektem - trzeba zmienić tylko tu ścieżkę
sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"

#wczytujemy jeszcze wczesniej dane nam potrzebne
strony_kandydatow<-readLines(paste0(sciezka,"slownik_fb_strony.txt"))
kandydaci<-readLines(paste0(sciezka,"slownik_fb_kandydaci.txt"))

#potrzebny wektor z unikalnymi id do pobrania danych ze strony z fb
id_kandydatow<-unlist(stri_extract_all_regex(strony_kandydatow,"[^/]+?$"))

#zmieniamy scieżkę, na tą gdzie pliki będziemy zapisywać
sciezka<-paste0(sciezka,"Facebook/")
dir.create(sciezka,showWarnings=FALSE)

##LIKES

#ilu mamy kandydatów + stworzenie wektora numerycznego dla liczby polubień
n<-length(strony_kandydatow)
lajki_fanpagow<-numeric(n)

#pętla do wychwycenia liczby polubień ze stron kandydatów
for(i in seq_len(n))
{
   #pomijamy kandydatów, którzy nie mają strony z liczbą polubień i dajemy im wartość 0
   if(!(strony_kandydatow[i]=="NA") && i!=12 && i!=14 && i!=16 && i!=21 && i!=24)
   {
      strona<-readLines(strony_kandydatow[i],warn=FALSE)
      lajki<-stri_extract_all_regex(strona,"(?<=Polubienia:</span>).+?(?=</span>)",simplify=TRUE)
      lajki<-lajki[which(!is.na(lajki))]
      lajki_fanpagow[i]<-as.numeric(stri_flatten(stri_extract_all_regex(lajki,"[0-9]+",simplify=TRUE)))
   }
   else
   {
      lajki_fanpagow[i]<-0
   }
}

#tworzymy folder, a następnie plik z wektorem liczby polubień
names(lajki_fanpagow)<-kandydaci
sciezka_lajki<-paste0(sciezka,"likes/")
dir.create(sciezka_lajki,showWarnings=FALSE)
adres<-paste(sciezka_lajki,"facebook_likes-",stri_replace_all_fixed(
   as.character(Sys.time()),":","-"),".txt",collapse="",sep="")
file.create(adres)
f<-file(adres,"w")
writeLines(stri_paste(kandydaci,collapse=";"),f) #otwieranie pliku przy pomocy read.csv2
writeLines(stri_paste(lajki_fanpagow,collapse=";"),f)
close(f)

##POSTY

#tworzymy ścieżkę do folderu z postami kandydatów
sciezka_posty<-paste0(sciezka,"posty/")

#pętla do wychwycenia postów ze stron kandydatów i zapisania ich do plików
for(i in seq_len(n))
{
   #Jeśli stona jest w wektorze stron jako NA lub nie było postów w danym dniu, to
   #  przypisujemy mu wektor pusty -> w pliku widoczne to jako "x"
   posty<-tryCatch(getPage(page=id_kandydatow[i],token=fb_oauth,n=100,since=paste0(Sys.Date(),"T00:00:01"),
      until=paste0(Sys.Date(),"T23:59:59")),error=function(err) character(0))
   sciezka_posty_kandydat<-paste0(sciezka_posty,kandydaci[i])
   dir.create(sciezka_posty_kandydat,showWarnings=FALSE)
   write.table(posty,paste0(sciezka_posty_kandydat,"/posty-",Sys.Date(),".txt"))
}
