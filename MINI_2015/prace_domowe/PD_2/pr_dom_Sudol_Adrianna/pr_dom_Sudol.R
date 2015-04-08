library(stringi)
library(XML)
library(rvest)
library(rjson)
library(ROAuth)
library(streamR)
library(stringr)
library(tm)
# Analiza tekstu ksiązki Emily Jane Bronte 'Wichrowe wzgórza' wraz z porównaniem 
# do dwóch najpopularniejszych wersji adapatacji filmowych z 1992 i 2011.
# Analizy filmow dokonam na podstawie polskich napisów odpowiadających danemu filmowi.

words=readLines('C:/Users/Adzia/Desktop/pr_dom_Sudol_Adrianna/stopwords.txt')#stopwordsy


### Wichrowe wzgorza - ksiazka ###
ksiazka=readLines('C:/Users/Adzia/Desktop/pr_dom_Sudol_Adrianna/Bronte-Emily-Wichrowe-Wzgórza.txt')
ksiazka=stri_trans_tolower(ksiazka)
cor=VCorpus(VectorSource(ksiazka))
cor=tm_map(cor, removeWords,words)#usuwamy stopwordsy
dtm <- DocumentTermMatrix(cor)
slowa=findFreqTerms(dtm,60)#wybieramy najczesciej wystepujace slowa
slowa=slowa[c(4,6,7,8,10,11,12,17,21)]#wyrzucam jeszcze 'recznie' rozne zbedne slowa
x=dtm
x <- t(x)
rs <- slam::row_sums(x)
l=length(slowa)
licz=vector()
for(i in 1:l){
  licz[i]=rs[slowa[i]]
}
slowa[which.max(licz)]#najczesciej wystepujacym slowem jest heathcliff, imie glownego bohatera
# czyli tak jak mozna bylo sie spodziewac
names(licz)=slowa

barplot(licz)# nalezy ogladac na zoom

### Wichrowe wzgórza - film 1992 ###
film1=readLines('C:/Users/Adzia/Desktop/pr_dom_Sudol_Adrianna/Wuthering Heights (1992).txt')
film1=stri_trans_tolower(film1)
film1=lapply(stri_extract_all_words(film1),function(x) x[-c(1:3)])
cor1=VCorpus(VectorSource(film1))
cor1=tm_map(cor1, removeWords,words)#usuwamy stopwordsy
dtm1 <- DocumentTermMatrix(cor1)
slowa1=findFreqTerms(dtm1,15)#wybieramy najczesciej wystepujace slowa
slowa1
slowa1=slowa1[c(2,3,6,11,14)]#wyrzucam jeszcze 'recznie' rozne zbedne slowa
x=dtm1
x <- t(x)
rs <- slam::row_sums(x)
l=length(slowa1)
licz1=vector()
for(i in 1:l){
  licz1[i]=rs[slowa1[i]]
}

slowa1[which.max(licz1)]#najczesciej wystepujacym slowem jest cathy, imie glownej bohaterki
# czyli widac ze jest roznica miedzy ksiazka, gdzie wiecej razy uzyto imienia mezczyzny 
# zas w filmie imie kobiety.
names(licz1)=slowa1

barplot(licz1)# nalezy ogladac na zoom



### Wichrowe wzgorza - film 2011 ###
film2=readLines('C:/Users/Adzia/Desktop/pr_dom_Sudol_Adrianna/Wuthering.Heights.2011.txt')
film2=stri_trans_tolower(film2)
film2=lapply(stri_extract_all_words(film2),function(x) x[-c(1,2)])
cor2=VCorpus(VectorSource(film2))
cor2=tm_map(cor2, removeWords,words)#usuwamy stopwordsy
dtm2 <- DocumentTermMatrix(cor2)
slowa2=findFreqTerms(dtm2,8)#wybieramy najczesciej wystepujace slowa
slowa2=slowa2[c(1,2,3,5)]#wyrzucam jeszcze 'recznie' rozne zbedne slowa
x=dtm2
x <- t(x)
rs <- slam::row_sums(x)
l=length(slowa2)
licz2=vector()
for(i in 1:l){
  licz2[i]=rs[slowa2[i]]
}

slowa2[which.max(licz2)]#najczesciej wystepujacym slowem jest cathy, imie bohaterki
# czyli tak jak w poprzednim filmie
names(licz2)=slowa2

barplot(licz2)

#polaczenie wszystkich danych:
licz_ks=licz[c(3,4,5,6,7)]
licz_film1=licz1
licz_film2=licz2
names(licz_ks)=stri_paste(names(licz[c(3,4,5,6,7)]),'(ks)')
names(licz_film1)=stri_paste(names(licz1),'(F1)')
names(licz_film2)=stri_paste(names(licz2),'(F2)')
barplot(c(licz_ks,licz_film1,licz_film2))

# Wnioski: Jak widac w ksiazce, ktorej autorem jest kobieta najczesciej powtarza
# sie imie glownego bohatera Heathcliff, zas w obydwu filmach w ktorych rezyserami
# byli mezczyzni, najczesciej powtarza sie imie glownej bohaterki Cathy.
# Moznaby wysunac wniosek ze plec autora ma znaczenie na wsytepowania danych slow
# (oczywsicie bardzo naciagany wniosek bo za malo danych).



