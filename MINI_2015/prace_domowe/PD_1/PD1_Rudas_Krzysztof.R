# Praca domowa1
# Porownanie filmow "Hobbit: Niezwykla podroz" i "Wladca pierscieni: Druzyna pierscienia"
# Obsada
library(rvest)
library(stringi)
library(XML)
#Obsada hobbit
html <-"http://www.imdb.com/title/tt0903624/fullcredits?ref_=tt_cl_sm#cast" 
tabele <- readHTMLTable(html, stringsAsFactors = FALSE)
tabele<-tabele[[3]]
tabele0<-na.omit(tabele[,2])
tabele0
#Obsada W³adca pierœcieni
html1 <-"http://www.imdb.com/title/tt0120737/fullcredits?ref_=tt_cl_sm#cast" 
tabele1 <- readHTMLTable(html1, stringsAsFactors = FALSE)
tabele1<-tabele1[[3]]
tabele10<-na.omit(tabele1[,2])
tabele10
#SprawdŸmy czy w obu filmach gra³ ten sam aktor
(aktorzy<-intersect(tabele0,tabele10))
#11 aktorów gra³o w obydwu filmach
wtab<-rep(0,11)
for (i in 1:11)
{
   wtab[i]<-which(tabele[,2]==aktorzy[i]) 
}

wtab1<-rep(0,11)
for (i in 1:11)
{
   wtab1[i]<-which(tabele1[,2]==aktorzy[i]) 
}

tabele[wtab,4]
tabele1[wtab1,4]

#Zatem pierwszych siedmiu aktorow, gralo te same role.

#Oceny widzow

#Wladca pierscieni
oceny <- html("http://www.imdb.com/title/tt0120737/ratings?ref_=tt_ov_rt")
oceny <- html_nodes(oceny, "td")
oceny<-html_text(oceny)
oceny1<-oceny[-1*(1:3)]
oceny1
liczba<-rep(0,10)
ocena1<-10:1
ocena<-stri_paste("Ocena ",ocena1)
ocena<-stri_paste(ocena,"-")
procent<-rep(0,10)
procent1<-rep(0,10)
for (i in 1:10)
{
   liczba[i]<-oceny1[(i-1)*3+1]
   procent1[i]<-unlist(stri_extract_all_regex(oceny1[(i-1)*3+2],"[0-9]*[0-9][\\.[0-9]]*%"))
   procent[i]<-unlist(stri_extract_all_regex(oceny1[(i-1)*3+2],"[0-9]*[0-9][\\.[0-9]]*"))
   
}

ocena<-stri_paste(ocena,procent1)
liczba<-as.numeric(liczba)
procent<-as.numeric(procent)
pie(procent,ocena,radius=1.0)
barplot(liczba,names.arg=ocena1)
(œrednia<-sum(liczba*ocena1)/sum(liczba))#8.68
wektor<-rep(0,sum(liczba))
licz<-0
for (i in 1:10)
{
   wektor[(1+licz):(licz+liczba[i])]<-10-i+1
   licz<-licz+liczba[i]
}
(mediana<-median(wektor))#9
#Hobbit
oceny2 <- html("http://www.imdb.com/title/tt0903624/ratings?ref_=tt_ov_rt")
oceny2 <- html_nodes(oceny2, "td")
oceny2<-html_text(oceny2)
oceny12<-oceny2[-1*(1:3)]
oceny12
liczba2<-rep(0,10)
ocena12<-10:1
ocena2<-stri_paste("Ocena ",ocena12)
ocena2<-stri_paste(ocena2,"-")
procent2<-rep(0,10)
procent12<-rep(0,10)
for (i in 1:10)
{
   liczba2[i]<-oceny12[(i-1)*3+1]
   procent12[i]<-unlist(stri_extract_all_regex(oceny12[(i-1)*3+2],"[0-9]*[0-9][\\.[0-9]]*%"))
   procent2[i]<-unlist(stri_extract_all_regex(oceny12[(i-1)*3+2],"[0-9]*[0-9][\\.[0-9]]*"))
   
}

ocena2<-stri_paste(ocena2,procent12)
liczba2<-as.numeric(liczba2)
procent2<-as.numeric(procent2)
pie(procent2,ocena2,radius=1.0)
barplot(liczba2,names.arg=ocena12)
(œrednia2<-sum(liczba2*ocena12)/sum(liczba2))#8.10
wektor2<-rep(0,sum(liczba2))
licz2<-0
for (i in 1:10)
{
   wektor2[(1+licz2):(licz2+liczba2[i])]<-10-i+1
   licz2<-licz2+liczba2[i]
}
(mediana2<-median(wektor2))#8

# Na podstawie przedstawionych wykresów i analizy podstawowych statystyk mo¿na
# stwierdziæ, ¿e Wladca pierscieni cieszy sie lepszymi opiniami niz Hobbit

# Oceny dla poszczegolnych grup wiekowych i plci

#Wladca Pierscieni
#Porównanie œrednich dla kobiet i mezczyzn
ocplci<-c(oceny[39],oceny[42])
ocplci<-as.numeric(unlist(stri_extract_all_regex(ocplci,"[0-9][\\.[0-9]]*")))
barplot(ocplci,names.arg=c("mezczyzni","kobiety"))
#Hobbit
ocplci2<-c(oceny2[39],oceny2[42])
ocplci2<-as.numeric(unlist(stri_extract_all_regex(ocplci2,"[0-9][\\.[0-9]]*")))
barplot(ocplci2,names.arg=c("mezczyzni","kobiety"))
#ogólnie nie ma istotnego zró¿nicowania ocen ze wzglêdu na plec, zarówno 
#dla mezczyzn jak i dla kobiet oceny œrednie Hobbita s¹ nieco nizsze 
#od srednich ocen wladcy pierscieni.

#Grupy wiekowe
#Wladca Pierscieni
srednieoceny<-rep(0,4)
for (i in 1:4)
{
   srednieoceny[i]<-oceny[45+(i-1)*9]
}
srednieoceny<-as.numeric(unlist(stri_extract_all_regex(srednieoceny,"[0-9][\\.[0-9]]*")))
barplot(srednieoceny,names.arg=c("Ponizej 18","18-29","30-44","45+"))
#Hobbit
srednieoceny2<-rep(0,4)
for (i in 1:4)
{
   srednieoceny2[i]<-oceny2[45+(i-1)*9]
}
srednieoceny2<-as.numeric(unlist(stri_extract_all_regex(srednieoceny2,"[0-9][\\.[0-9]]*")))
barplot(srednieoceny2,names.arg=c("Ponizej 18","18-29","30-44","45+"))

# Analizuj¹c grupy wiekowe, mozna dojsc do wniosku, ze im mlodsza grupa widzow,
# tym srednio lepsze opinie.

#Analiza ocen w zaleznosci od wieku mezczyzn
#Wladca pierscieni
srednieocenymez<-rep(0,4)
srednieocenykob<-rep(0,4)
oceny
for (i in 1:4)
{
   srednieocenymez[i]<-oceny[48+(i-1)*9]
   srednieocenykob[i]<-oceny[51+(i-1)*9]
}
srednieocenymez<-as.numeric(unlist(stri_extract_all_regex(srednieocenymez,"[0-9][\\.[0-9]]*")))
srednieocenykob<-as.numeric(unlist(stri_extract_all_regex(srednieocenykob,"[0-9][\\.[0-9]]*")))
barplot(srednieocenymez,names.arg=c("Ponizej 18","18-29","30-44","45+"))
barplot(srednieocenykob,names.arg=c("Ponizej 18","18-29","30-44","45+"))
#Hobbit
srednieocenymez2<-rep(0,4)
srednieocenykob2<-rep(0,4)
oceny
for (i in 1:4)
{
   srednieocenymez2[i]<-oceny2[48+(i-1)*9]
   srednieocenykob2[i]<-oceny2[51+(i-1)*9]
}
srednieocenymez2<-as.numeric(unlist(stri_extract_all_regex(srednieocenymez2,"[0-9][\\.[0-9]]*")))
srednieocenykob2<-as.numeric(unlist(stri_extract_all_regex(srednieocenykob2,"[0-9][\\.[0-9]]*")))
barplot(srednieocenymez2,names.arg=c("Ponizej 18","18-29","30-44","45+"))
barplot(srednieocenykob2,names.arg=c("Ponizej 18","18-29","30-44","45+"))

# W poszczegolnych grupach wiekowych plec ma znow niewielkie znaczenie odnosnie oceny filmow.
# Dla obydwu filmow obserwujemy zarowno u kobiet jak i u mezczyzn spadek wartosci ocen 
# wraz ze wzrostem wieku.

