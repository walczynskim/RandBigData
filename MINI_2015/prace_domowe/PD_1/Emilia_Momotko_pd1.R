library(XML)
library(rvest)
library(stringi)
library(ggplot2)

#Piecdziesiat twarzy greya

#imdb
grey<-html("http://www.imdb.com/title/tt2322441/")

#ocena
oc_grey<-as.numeric(html_text(html_nodes(grey,"strong span")))

#data premiery w Polsce

data_premiery_grey<-stri_replace_all_fixed(stri_trim_both(html_text(html_nodes(grey,".infobar .nobr a"))),"\n"," ")

#rezyser
rezyser_grey<-unlist(stri_extract_all_regex(html_text(html_nodes(grey,"p+ .txt-block")),"(?<=Director:\\n).+?(?=\n)"))

#opis
opis_grey<-stri_extract_all_regex(html_text(html_nodes(grey,"#overview-top p")),"(?<=\\n).+")[[2]]

############

link_grey<-"http://www.imdb.com" #przyda sie pozniej

#korzystam z tabelek ocen dosteonych na imdb
#tabelki
oceny_grey<-html_attr(html_nodes(grey,".link"),"href") #wybieramy odpowiednie linki
oceny_grey<-oceny_grey[stri_detect_fixed(oceny_grey,"ratings")] #interesuje nas tylko linkz ocenami]
LINK_do_ocen_grey<-stri_flatten(c(link_grey,oceny_grey))# juz caly link

#rozklad glosow
rozklad_glosow_grey<-as.data.frame(readHTMLTable(LINK_do_ocen_grey,
                                                 stringsAsFactors = FALSE)[[1]])
names(rozklad_glosow_grey)[2]<-"Percentage" #zmieniam nazwy kolumn, bo nie do konca sa prawidlowe
names(rozklad_glosow_grey)[3]<-"Rating"

#zmieniam wartosci w kolumnie 2 -oczyszczam dane
rozklad_glosow_grey[,2]<-unlist(stri_extract_all_regex(rozklad_glosow_grey[,2],
                                                       "[0-9]{1,}\\.[0-9]{1,}"))

#chce, aby dane w kolumnach  byly numeryczne
rozklad_glosow_grey<-as.data.frame(apply(rozklad_glosow_grey,2,as.numeric))

#Raporty ocen
#podobnie jak wczesniej wczytuje tabelke i oczyszczam dane
raport_grey<-data.frame(readHTMLTable(LINK_do_ocen_grey,stringsAsFactors = FALSE)[[2]])
names(raport_grey)[1]<-"Kategoria"
n<-nrow(raport_grey)
raport_grey<-raport_grey[-(n-1),] #w tabeli znajduje sie pusty wiersz
x<-apply(raport_grey[,c(2,3)],2,function(y){
  
  as.numeric(unlist(stri_extract_all_regex(y,"[0-9]{1,}(\\.)?([0-9]{0,})")))
})
raport_grey[,2]<-x[,1]
raport_grey[,3]<-x[,2]
row.names(raport_grey)<-NULL

#Limitless

#imdb
limit<-html("http://www.imdb.com/title/tt1219289/?ref_=fn_al_tt_1")

#ocena
oc_limit<-as.numeric(html_text(html_nodes(limit,"strong span")))


#data premiery w Polsce

data_premiery_limit<-stri_replace_all_fixed(stri_trim_both(html_text(html_nodes(limit,".infobar .nobr a"))),"\n"," ")

#rezyser
rezyser_limit<-unlist(stri_extract_all_regex(html_text(html_nodes(limit,"p+ .txt-block")),"(?<=Director:\\n).+?(?=\n)"))

#opis
opis_limit<-stri_extract_all_regex(html_text(html_nodes(limit,"#overview-top p")),"(?<=\\n).+")[[2]]

############

link_limit<-"http://www.imdb.com" #przyda sie pozniej

#korzystam z tabelek ocen dosteonych na imdb
#tabelki
oceny_limit<-html_attr(html_nodes(limit,".link"),"href") #wybieramy odpowiednie linki
oceny_limit<-oceny_limit[stri_detect_fixed(oceny_limit,"ratings")] #interesuje nas tylko linkz ocenami]
LINK_do_ocen_limit<-stri_flatten(c(link_limit,oceny_limit))# juz caly link

#rozklad glosow
rozklad_glosow_limit<-as.data.frame(readHTMLTable(LINK_do_ocen_limit,
                                                 stringsAsFactors = FALSE)[[1]])
names(rozklad_glosow_limit)[2]<-"Percentage" #zmieniam nazwy kolumn, bo nie do konca sa prawidlowe
names(rozklad_glosow_limit)[3]<-"Rating"

#zmieniam wartosci w kolumnie 2 -oczyszczam dane
rozklad_glosow_limit[,2]<-unlist(stri_extract_all_regex(rozklad_glosow_limit[,2],
                                                       "[0-9]{1,}\\.[0-9]{1,}"))

#chce, aby dane w kolumnach  byly numeryczne
rozklad_glosow_limit<-as.data.frame(apply(rozklad_glosow_limit,2,as.numeric))

#Raporty ocen
#podobnie jak wczesniej wczytuje tabelke i oczyszczam dane
raport_limit<-data.frame(readHTMLTable(LINK_do_ocen_limit,stringsAsFactors = FALSE)[[2]])
names(raport_limit)[1]<-"Kategoria"
n<-nrow(raport_limit)
raport_limit<-raport_limit[-(n-1),] #w tabeli znajduje sie pusty wiersz
x<-apply(raport_limit[,c(2,3)],2,function(y){
  
  as.numeric(unlist(stri_extract_all_regex(y,"[0-9]{1,}(\\.)?([0-9]{0,})")))
})
raport_limit[,2]<-x[,1]
raport_limit[,3]<-x[,2]
row.names(raport_limit)<-NULL



##############################################################################
#porownanie Greya i Limitless z imdb

#zainteresowanie
#wzgledem plci
wartosci1_grey<-stri_paste(round(c(raport_grey[1,2],raport_grey[2,2])/sum(raport_grey[c(1,2),2])*100,2),"%")
labels1_grey<-stri_paste(raport_grey[1:2,1],":",wartosci1_grey)
wartosci1_limit<-stri_paste(round(c(raport_limit[1,2],raport_limit[2,2])/sum(c(raport_limit[1,2],raport_limit[2,2]))*100,2),"%")
labels1_limit<-stri_paste(raport_limit[1:2,1],":",wartosci1_limit)

pie(raport_limit[1:2,2],labels=labels1_limit)
title("Limitless")

pie(raport_grey[1:2,2],labels=labels1_grey)
title("Fifty Shades of Grey")
#film Limitless cieszyl sie zdecydowanie wiekszym zainteresowaniem mezczyzn
#niz kobiet, natomiast w przypadku Fifty shDES of Grey zainteresowanie
#rozlozylo sie po rowno

#wzgledem wieku
wartosci2_grey<-stri_paste(round(raport_grey[c(3,6,9,12),2]/sum(raport_grey[c(3,6,9,12),2])*100,2),"%")
labels2_grey<-stri_paste(raport_grey[c(3,6,9,12),1],":",wartosci2_grey)
wartosci2_limit<-stri_paste(round(raport_limit[c(3,6,9,12),2]/sum(raport_limit[c(3,6,9,12),2])*100,2),"%")
labels2_limit<-stri_paste(raport_limit[c(3,6,9,12),1],":",wartosci2_limit)

pie(raport_limit[c(3,6,9,12),2],labels=labels2_limit)
title("Limitless")
pie(raport_grey[c(3,6,9,12),2],labels=labels2_grey) #wiek ogolnie
title("Fifty Shades of Grey")
#oba filmy ogladali glownie mlodzi ludzie

#u kobiet
wartosci3_grey<-stri_paste(round(raport_grey[c(5,8,11,14),2]/sum(raport_grey[c(5,8,11,14),2])*100,2),"%")
labels3_grey<-stri_paste(raport_grey[c(5,8,11,14),1],":",wartosci3_grey)
wartosci3_limit<-stri_paste(round(raport_limit[c(5,8,11,14),2]/sum(raport_limit[c(5,8,11,14),2])*100,2),"%")
labels3_limit<-stri_paste(raport_limit[c(5,8,11,14),1],":",wartosci3_limit)

pie(raport_limit[c(5,8,11,14),2],labels=labels3_limit)
title("Limitless")

pie(raport_grey[c(5,8,11,14),2],labels=labels3_grey)
title("Fifty Shades of Grey")
#oba filmy ogladaly glownie kobiety przd 30-tka

#u mezczyzn
wartosci4_grey<-stri_paste(round(raport_grey[c(4,7,10,13),2]/sum(raport_grey[c(4,7,10,13),2])*100,2),"%")
labels4_grey<-stri_paste(raport_grey[c(4,7,10,13),1],":",wartosci4_grey)
wartosci4_limit<-stri_paste(round(raport_limit[c(4,7,10,13),2]/sum(raport_limit[c(4,7,10,13),2])*100,2),"%")
labels4_limit<-stri_paste(raport_limit[c(4,7,10,13),1],":",wartosci4_limit)

pie(raport_limit[c(4,7,10,13),2],labels=labels4_limit) #wsrod mezczyzn
title("Limitless")

pie(raport_grey[c(4,7,10,13),2],labels=labels4_grey)
title("Fifty Shades of Grey")
#wnioski jak wyzej

#srednie oceny w kategoriach

#plec:
average<-rbind(raport_grey[1:2,3],raport_limit[1:2,3])

#porownanie ocen mezczyzn i kobiet
mp <- barplot(average,width=0.1,space=c(0,0.5), ylim=c(0,11.9),col=c("lightcoral","royalblue"),beside = TRUE, names.arg = raport_grey[1:2,1])
text(mp, average, labels = format(average, 2),
     pos = 3, cex = .75)
legend("topright", 
       legend = c("Fifty Shades Of Grey","Limitless"), 
       fill = c("lightcoral","royalblue"),cex=0.7)

#mezczyzni bardziej krytyczni w stosunku do Greya, film Limitless cieszyl
#sie jednakowa ocena wsrod plci

#porownanie ocen wsrod kobiet

average_kobiety<-rbind(raport_grey[c(5,8,11,14),3],raport_limit[c(5,8,11,14),3])
mp2 <- barplot(average_kobiety,width=0.1,space=c(0,0.2), ylim=c(0,11.9),col=c("lightcoral","royalblue"),beside = TRUE, names.arg = raport_grey[c(5,8,11,14),1])
text(mp2, average_kobiety, labels = format(average_kobiety, 2),
     pos = 3, cex = .75)
legend("topright", 
       legend = c("Fifty Shades Of Grey","Limitless"), 
       fill = c("lightcoral","royalblue"),cex=0.7)
#film Limitless - oceny na takim samym poziomie, Grey-wraz ze wzrostem wieku ocena
#znaczaco spada

#porownanie ocen wsrod m
averagem<-rbind(raport_grey[c(4,7,10,13),3],raport_limit[c(4,7,10,13),3])
mp3 <- barplot(averagem,width=0.1,space=c(0,0.2), ylim=c(0,11.9),col=c("lightcoral","royalblue"),beside = TRUE, names.arg = raport_grey[c(4,7,10,13),1])
legend("topright", 
       legend = c("Fifty Shades Of Grey","Limitless"), 
       fill = c("lightcoral","royalblue"),cex=0.7)
text(mp3, averagem, labels = format(averagem, 4),
     pos = 3, cex = .75)
#mezczyzni nie przepadaja za Greyem, Limitless zdecydowanie lepiej oceniany
#niemal rozklad jednostajny

#ogolnie wiek
averagew<-rbind(raport_grey[c(3,6,9,12),3],raport_limit[c(3,6,9,12),3])
mp4 <- barplot(averagew,width=0.1,space=c(0,0.2), ylim=c(0,11.9),col=c("lightcoral","royalblue"),beside = TRUE, names.arg = raport_grey[c(3,6,9,12),1])
legend("topright", 
       legend = c("Fifty Shades Of Grey","Limitless"), 
       fill = c("lightcoral","royalblue"),cex=0.7)
text(mp4, averagew, labels = format(averagew, 4),
     pos = 3, cex = .75)
#wraz z wiekiem oceny spadaja, Limitless zdecydowanie lepszy, niemal rozklad
#jednostajny

pie(rozklad_glosow_grey[,1],labels=rozklad_glosow_grey[,3])
#widzimy ze najczestsza ocena jest 10 i 1-zdecydowanie kontrowersyjny film
pie(rozklad_glosow_limit[,1],labels=rozklad_glosow_limit[,3])
#najczestsze oceny: 7,8,9 - film bardzo dobry


#oc_grey < oc_limit - Grey slabszy

############################################################################

#filmweb - grey
grey_f<-html("http://www.filmweb.pl/film/Pi%C4%99%C4%87dziesi%C4%85t+twarzy+Greya-2015-655761")
do_obsady_f<-html_attr(html_nodes(grey_f,".seeAllCast"),"href") #link do obsady

glowny_f<-"http://www.filmweb.pl" #przyda sie
link_f<-html(stri_paste(glowny_f,do_obsady_f))


ob_f<-html_text(html_nodes(link_f,".filmCast a")) #obsada
ob_f<-unique(ob_f) #dubluja sie niektore, usuwamy
ob_f<-ob_f[stri_detect_regex(ob_f,"[A-Z]")] #biore tylko nazwiska
pos_f<-html_text(html_nodes(link_f,"td~ td span:nth-child(1)"))#postaci
n<-length(pos_f)
pos_f<-pos_f[seq(1,n,by=2)] #biore tylko to co wazne

x<-html_attr(html_nodes(link_f,"td+ td > a"),"href") #linki do aktorow

#zlaczam z linkiem glownym
a<-unlist(lapply(x,function(y){
  stri_paste(glowny_f,y)
}))

#tutaj zbieram i oczyszczam dane od razu
oceny<-sapply(a,function(y){
  
  k<-as.numeric(stri_replace_all_fixed(unlist(stri_extract_all_regex(html_text(html_nodes(html(y),"#pr_6")),"[0-9][0-9]?,[0-9]")),",","."))
  if((identical(k,numeric(0)))){
    return(NA)
  } else return(k)
  
})


#zmieniam na wektor ocen
oceny<-unlist(unname(oceny))

#wydobywam daty urodzenia
daty_urodzenia<-sapply(a,function(y){
  
  k<-html_text(html_nodes(html(y),".bottom-15 td"))
  if((identical(k,character(0)))){
    return(NA)
  }
  stri_extract_all_regex(k,"[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}")[[1]]
  
})


daty_urodzenia<-strptime(unname(unlist(daty_urodzenia)),format="%Y-%m-%d")

wiek<-floor((as.POSIXct(Sys.Date())-daty_urodzenia)/365)
wiek<-as.numeric(wiek)
#mamy ramke danych z aktorami, postaciami i ocenami
ramka_grey<-data.frame(aktor=ob_f,postac=pos_f,ocena=oceny,data_urodzenia=daty_urodzenia,wiek=wiek)

mean(ramka_grey[,3],na.rm=TRUE) #srednia ocena
mean(ramka_grey[,5],na.rm=TRUE) #sredni wiek

#Limitless

#filmweb
limit_f<-html("http://www.filmweb.pl/film/Jestem+Bogiem-2011-548252")
do_obsady_f<-html_attr(html_nodes(limit_f,".seeAllCast"),"href") #link do obsady

glowny_f<-"http://www.filmweb.pl" #przyda sie
link_f<-html(stri_paste(glowny_f,do_obsady_f))


ob_f<-html_text(html_nodes(link_f,".filmCast a")) #obsada
ob_f<-unique(ob_f) #dubluja sie niektore, usuwamy
ob_f<-ob_f[stri_detect_regex(ob_f,"[A-Z]")] #biore tylko nazwiska
pos_f<-html_text(html_nodes(link_f,"td~ td span:nth-child(1)"))#postaci
n<-length(pos_f)
pos_f<-pos_f[seq(1,n,by=2)] #biore tylko to co wazne

x<-html_attr(html_nodes(link_f,"td+ td > a"),"href") #linki do aktorow

#zlaczam z linkiem glownym
a<-unlist(lapply(x,function(y){
  stri_paste(glowny_f,y)
}))

#tutaj zbieram i oczyszczam dane od razu
oceny<-sapply(a,function(y){
  
  k<-as.numeric(stri_replace_all_fixed(unlist(stri_extract_all_regex(html_text(html_nodes(html(y),"#pr_6")),"[0-9][0-9]?,[0-9]")),",","."))
  if((identical(k,numeric(0)))){
    return(NA)
  } else return(k)
  
})


#zmieniam na wektor ocen
oceny<-unlist(unname(oceny))

#wydobywam daty urodzenia
daty_urodzenia<-sapply(a,function(y){
  
  k<-html_text(html_nodes(html(y),".bottom-15 td"))
  if((identical(k,character(0)))){
    return(NA)
  }
  stri_extract_all_regex(k,"[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}")[[1]]
  
})


daty_urodzenia<-strptime(unname(unlist(daty_urodzenia)),format="%Y-%m-%d")


wiek<-floor((as.POSIXct(Sys.Date())-daty_urodzenia)/365)
wiek<-as.numeric(wiek)
#mamy ramke danych z aktorami, postaciami i ocenami
ramka_limit<-data.frame(aktor=ob_f,postac=pos_f,ocena=oceny,data_urodzenia=daty_urodzenia,wiek=wiek)

mean(ramka_limit[,3],na.rm=TRUE) #srednia ocena
mean(ramka_limit[,5],na.rm=TRUE) #sredni wiek


#porownanie wieku aktorow:

par(mfrow=c(1,2))
boxplot(ramka_limit[,5])
boxplot(ramka_grey[,5])

n1<-nrow(ramka_grey)
n2<-nrow(ramka_limit)

pom<-data.frame(c(rep("Grey",n1),rep("Limitless",n2)),c(ramka_grey[,5],ramka_limit[,5]))
names(pom)[1]<-"film"
names(pom)[2]<-"wiek"
ggplot(data = pom, aes(x=film, y=wiek)) + geom_boxplot(aes(fill=film),na.rm=TRUE)

mean(ramka_limit[,5],na.rm=TRUE)
mean(ramka_grey[,5],na.rm=TRUE) #sredni wiek

#sredni wiek
#widzimy ze w filmie Piecdziesiat Twzrzy Greya obsada jest stosunkowo m³oda,
#miediana wieku nie przekracza 35 lat
#z kolei w filmie limitless sytuacja wyglada inaczej, aktorzy sa bardziej
#dojrzalymi ludzmi, mozliwe ze bardziej doswiadczonymi co moze rowniez wplywac na
#rozklad ocen filmów oraz na rozklad ocen aktorów grajacych w filmach


#porownania ocen aktorow
pom2<-data.frame(c(rep("Grey",n1),rep("Limitless",n2)),c(ramka_grey[,3],ramka_limit[,3]))
names(pom2)[1]<-"film"
names(pom2)[2]<-"ocena"

ggplot(data = pom2, aes(x=film, y=ocena)) + geom_boxplot(aes(fill=film),na.rm=TRUE)
#oakzuje sie jednak ze mediana ocen poszczegonych aktorow z filmu Piezdziesiat
#Twarzy Greya jest bardzo zblizona do mediany ocen poszczegonych aktorow z filmu
#Limitless. W filmie Limitless zauwazamy wiekszy rozstep miedzy ocenami, w tym filmie
#zatem grali roznorodni aktorzy, zarowno Ci doceniani przez spolecznosc filmwebu jak
#i nie.