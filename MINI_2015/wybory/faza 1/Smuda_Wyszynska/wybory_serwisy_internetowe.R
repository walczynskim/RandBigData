############################################################################
#############            SERWISY INTERNETOWE        ########################
############################################################################
#Ustalamy ścieżkę do katalogu "wybory"
path<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/"


#Ładujemy potrzebne pakiety
library("rvest")
library("stringi")
library("XML")



#########################    GOOGLE NEWS    ################################
#Artykuły na tym portalu należą do różnych stron, a każda z nich ma swoją
#organizację tekstu, dlatego poniższy kod wyciąga cały kod HTML strony
#z artykułem i zlicza ile razy występują w nim nazwiska kandydatów

#Wybieramy klucze, po których będziemy szukać linków do artykułów
slowa_klucze<-c("Braun","Duda","Dudy","Dudę","Dude","Dudą","Dudzie",
      "Grodzka","Grodzką","Grodzkiej","Jankowski","Jarubas","Jędrzejewski",
      "Jedrzejewski","Korab-Karpowicz","Komorowski","Korwin",
      "Kowalski","Kukiz","Łaska","Laska","Majdański","Majdanski","Marzec",
      "Morawiecki","Nowak","Nowicka","Ogórek","Ogorek","Palikot","Piątek",
      "Piatek","Słomka","Slomka","Tanajno","Tanajnym","Tanajnego",
      "Tanajnemu","Wilk","Zydorczak","wybory","prezydent","kandydat")
klucz<-stri_paste(slowa_klucze,collapse="|")

#Zczytujemy stronę z newsami i zbieramy linki
suppressWarnings(Strona<-readLines("http://news.google.pl/"))
Linki<-unlist(stri_match_all_regex(Strona,'(?<=url=").+?(?=" id=|" ssid)'))
Linki<-Linki[which(!is.na(Linki))]

#Wybieramy tylko te linki, które dotyczą wyborów
dobre_artykuly<-unlist(stri_match_all_regex(Linki,klucz))
dobre_artykuly<-which(!is.na(dobre_artykuly))
Linki<-Linki[dobre_artykuly]

#Poprawiamy klucze na tokeny, by nie rozróżniać odmian jednego nazwiska
slowa_klucze<-c("Braun","Dud","Grodzk","Jankowski","Jarubas","Jędrzejewski",
      "Karpowicz","Komorowski","Korwin","Kowalski","Kukiz","Łaska",
      "Majdański","Marzec","Morawiecki","Nowak","Nowick","Ogórek","Palikot",
      "Piątek","Słomk","Tanajn","Wilk","Zydorczak") #Propozycja tokenow
klucz<-stri_paste(slowa_klucze,collapse="|")

#Otwieramy strony z artykułami i zliczamy ilość występowania nazwiska kandydatów
n<-length(Linki)
artykuly_kandydaci<-vector("list",n)
for(i in seq_len(n)){
  suppressWarnings(Artykul<-readLines(Linki[i]))
  kandydaci<-unlist(stri_match_all_regex(Artykul,klucz))
  artykuly_kandydaci[[i]]<-kandydaci[which(!is.na(kandydaci))]
}

wyniki<-table(unlist(artykuly_kandydaci))
#Zmieniamy tokeny na pełne nazwiska
tokeny<-names(wyniki)
for(i in seq_along(tokeny)){
   if(tokeny[i]=="Dud") tokeny[i]="Duda"
   if(tokeny[i]=="Grodzk") tokeny[i]="Grodzka"
   if(tokeny[i]=="Nowick") tokeny[i]="Nowicka"
   if(tokeny[i]=="Słomk") tokeny[i]="Słomka"
   if(tokeny[i]=="Tanajn") tokeny[i]="Tanajno"
}
names(wyniki)<-tokeny

#Zapisujemy nasz wynik do pliku
wyniki1<-stri_paste(tokeny,as.integer(wyniki),collapse=" ",sep=" ")
name<-paste(path,"GoogleNews/GoogleNews",stri_replace_all_fixed(
   as.character(Sys.time()),":","-"),".txt",collapse="",sep="")
if(!file.exists(stri_paste(path,"/GoogleNews"))){dir.create(
   stri_paste(path,"/GoogleNews"))}
file.create(name)
f<-file(name,"w")
writeLines(stri_paste(tokeny,collapse=" "),f)
writeLines(stri_paste(as.integer(wyniki),collapse=" "),f)
close(f)


######################          ONET       ################################
#Korzystamy ze strony informacyjnej Onetu z wyselekcjonowanymi informacjami
#na temat wyborów. Artykuły, które są tam podlinkowane mają podobną
#strukturę, dlatego wyciągamy z nich cały tekst artykułu i zapisujemy go
#do pliku.

#Pobieramy linki do artykułów
onet<-"http://wiadomosci.onet.pl/wybory-prezydenckie-2015"
Strona<-readLines(onet)
TytulyLinki<-unlist(stri_match_all_regex(
   Strona,'(?<=href="http://wiadomosci.onet.pl/).+?(?=" title=")'))
TytulyLinki<-TytulyLinki[which(!is.na(TytulyLinki))]

#Ściagamy teksty poszczególnych artykułów
#Zapisujemy je w folderze z datą i godziną pobrania
name<-paste(path,"Onet/",stri_replace_all_fixed(
   as.character(Sys.time()),":","-"),collapse="",sep="")
if(!file.exists(stri_paste(path,"Onet"))){dir.create(stri_paste(path,"Onet"))}
dir.create(name)

for(i in seq_along(TytulyLinki)){
   Strona<-readLines(stri_paste("http://wiadomosci.onet.pl/",TytulyLinki[i]))
   TytulArtykulu<-unlist(stri_match_all_regex(
      Strona,"(?<=<title>).+?(?=</title>)"))
   TytulArtykulu<-TytulArtykulu[which(!is.na(TytulArtykulu))]
   Tresc<-unlist(stri_match_all_regex(
      Strona,'(?<=<p class=\"hyphenate\">).+?(?=</p>)'))
   Tresc<-Tresc[which(!is.na(Tresc))]
   name1<-paste(name,"/",i,".txt",
      collapse="",sep="")
   file.create(name1)
   f<-file(name1,"w")
   writeLines(stri_paste(TytulArtykulu,"\n\n"),f)
   writeLines(Tresc,f)
   close(f)
}



##################      WIRTUALNA POLSKA      ###############################
#Korzystamy ze strony informacyjnej Wirtualnej Polski z wyselekcjonowanymi
#informacjami na temat wyborów. Artykuły, które są tam podlinkowane mają podobną
#strukturę, dlatego wyciągamy z nich cały tekst artykułu i zapisujemy go
#do pliku.

#Pobieramy linki do artykułów
wp<-"http://wiadomosci.wp.pl/kat,140394,title,Wybory-prezydenckie-w-2015-r,raport.html"
Strona<-html(wp)
StronaWezly<-html_nodes(Strona,"#bxRaportSpecjalny h2 a")
StronaLinki<-html_attr(StronaWezly,"href")

#Ściagamy teksty poszczególnych artykułów
#Zapisujemy je w folderze z datą i godziną pobrania
name<-paste(path,"WirtualnaPolska/",stri_replace_all_fixed(
   as.character(Sys.time()),":","-"),collapse="",sep="")
if(!file.exists(stri_paste(path,"WirtualnaPolska"))){
   dir.create(stri_paste(path,"WirtualnaPolska"))}
dir.create(name)

for(i in seq_along(StronaLinki)){
   Strona<-html(stri_paste("http://wiadomosci.wp.pl",StronaLinki[i]))
   TytulArtykuluWezel<-html_nodes(Strona,"h1")
   TytulArtykulu<-html_text(TytulArtykuluWezel)
   TextWezly<-html_nodes(Strona,"#intertext1 , .lead")
   Text<-html_text(TextWezly)
   name1<-paste(name,"/",i,".txt",
      collapse="",sep="")
   file.create(name1)
   f<-file(name1,"w")
   writeLines(stri_paste(TytulArtykulu,"\n\n"),f)
   writeLines(Text,f)
   close(f)
}



########################        INTERIA           ##########################
#Korzystamy ze strony informacyjnej Interii z wyselekcjonowanymi
#informacjami na temat wyborów. Artykuły, które są tam podlinkowane mają podobną
#strukturę, dlatego wyciągamy z nich cały tekst artykułu i zapisujemy go
#do pliku.

#Pobieramy linki do artykułów
interia<-"http://fakty.interia.pl/raport-wybory-prezydenckie-2015"
Strona<-html(interia)
StronaWezly<-html_nodes(Strona,"#brief_list_1 .brief-title-link")
StronaLinki<-html_attr(StronaWezly,"href")

#Ściagamy teksty poszczególnych artykułów
#Zapisujemy je w folderze z datą i godziną pobrania
name<-paste(path,"Interia/",stri_replace_all_fixed(
   as.character(Sys.time()),":","-"),collapse="",sep="")
if(!file.exists(stri_paste(path,"Interia"))){
   dir.create(stri_paste(path,"Interia"))}
dir.create(name)

for(i in seq_along(StronaLinki)){
   Strona<-html(stri_paste("http://fakty.interia.pl",StronaLinki[i]))
   TytulArtykuluWezel<-html_nodes(Strona,"#articleSingle1 h1")
   TytulArtykulu<-html_text(TytulArtykuluWezel)
   TextWezly<-html_nodes(Strona,".fontSize-medium , #articleSingle1 p")
   Text<-html_text(TextWezly)
   name1<-paste(name,"/",i,".txt",
      collapse="",sep="")
   file.create(name1)
   f<-file(name1,"w")
   writeLines(stri_paste(TytulArtykulu,"\n\n"),f)
   writeLines(Text,f)
   close(f)
}
