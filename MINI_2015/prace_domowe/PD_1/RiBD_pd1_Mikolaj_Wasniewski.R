#### Mikolaj Wasniewski
## praca domowa 1
library(rvest)
library(stringi)
library(XML)
## http://www.imdb.com/title/tt1796960/episodes
#porównanie odcinkow serialu HOMELAND sezon 4:

html<-html("http://www.imdb.com/title/tt1796960/episodes")
odcinki<-html_nodes(html,".info a")
odcinki<-html_attr(odcinki,"href")
n<-length(odcinki)
ocena<-numeric(n)
oceny_kobiet<-numeric(n)
oceny_mezczyzn<-numeric(n)

tytul<-character(n)
for (i in 1:n){
   adres<-stri_paste("http://www.imdb.com",odcinki[i])
   html1<-html(adres)
   ocena1 <- html_nodes(html1, "div.titlePageSprite")

   tytul[i]<-html_text(html_nodes(html1,".header .itemprop")) #tytul odcinka
   ocena[i]<-as.numeric(html_text(ocena1))#³aczna ocena odcinka
   
   #szczegolowe oceny odcinkow
   oceny<-html_attr(html_nodes(html1,".mellow+ a"),"href")
   oceny_laczne<-readHTMLTable(
      stri_paste(
         stri_extract(adres,regex="http://www.imdb.com/title/[a-zA-Z0-9]+/")
         ,oceny))[[2]]
   oceny_laczne[,2]<-stri_sub(oceny_laczne[,2],3)
   oceny_laczne[,3]<-stri_sub(oceny_laczne[,3],3)
   oceny_kobiet[i]<-oceny_laczne[2,3]
   oceny_mezczyzn[i]<-oceny_laczne[1,3]
}
Homeland_info<-data.frame(tytul_odcinka=tytul,ocena_laczna=ocena,
                          ocena_kobiet=oceny_kobiet,
                          ocena_mezczyzn=oceny_mezczyzn)
Homeland_info
barplot(Homeland_info$ocena_laczna,ylim=c(0,10),xlab="odcinki",main="ocena laczna")

#mozna zauwazyc ze ocena ostatniego odcinka jest duzo nizsza niz ocena pozostalych 
#odcnkow, oceny kobiet i mezczyzn sa porownywalne.

#nastepnie zrobi³em to samo tylko dla 3 sezonu serialu House of Cards:

html<-html("http://www.imdb.com/title/tt1856010/episodes")
odcinki<-html_nodes(html,".info a")
odcinki<-html_attr(odcinki,"href")
n<-length(odcinki)
ocena<-numeric(n)
oceny_kobiet<-numeric(n)
oceny_mezczyzn<-numeric(n)

tytul<-character(n)
for (i in 1:n){
   adres<-stri_paste("http://www.imdb.com",odcinki[i])
   html1<-html(adres)
   ocena1 <- html_nodes(html1, "div.titlePageSprite")
   
   tytul[i]<-html_text(html_nodes(html1,".header .itemprop")) #tytul odcinka
   ocena[i]<-as.numeric(html_text(ocena1))#³aczna ocena odcinka
   
   #szczegolowe oceny odcinkow
   oceny<-html_attr(html_nodes(html1,".mellow+ a"),"href")
   oceny_laczne<-readHTMLTable(
      stri_paste(
         stri_extract(adres,regex="http://www.imdb.com/title/[a-zA-Z0-9]+/")
         ,oceny))[[2]]
   oceny_laczne[,2]<-stri_sub(oceny_laczne[,2],3)
   oceny_laczne[,3]<-stri_sub(oceny_laczne[,3],3)
   
   oceny_kobiet[i]<-oceny_laczne[2,3]
   oceny_mezczyzn[i]<-oceny_laczne[1,3]
}

House_of_Cards_info<-data.frame(tytul_odcinka=tytul,ocena_laczna=ocena,
                          ocena_kobiet=oceny_kobiet,
                          ocena_mezczyzn=oceny_mezczyzn)
House_of_Cards_info
barplot(House_of_Cards_info$ocena_laczna,ylim=c(0,10),xlab="odcinki",main="ocena laczna")

# laczna ocena poszczególnych odcinkow jest porównywalna dla wszystkich 
# odcinkow


mean(as.numeric(House_of_Cards_info$ocena_laczna))-
   mean(as.numeric(Homeland_info$ocena_laczna))

# srednia ocene dla serialu House of Cards jest wyzsza od srednie oceny 
# serialu Homeland o oko³o 0,4


