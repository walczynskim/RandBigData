library(stringi)
library(XML)
library(rvest)
library(rjson)
library(twitteR)
library(ROAuth)
library(streamR)

slownik=readLines('D://szkola//R i Big Data//projekt 1//slownik.txt')
l=length(slownik)
data=as.character(Sys.Date())
##################### TVN ###########################

tvn=html("http://www.tvn24.pl")
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//tvn//',data,'//tresc',godz,'.txt')
writeLines(as(tvn,'character'),nazwa)
cast <- html_nodes(tvn, 'a')
tytuly=html_text(cast)
linki=html_attrs(cast)
all=list()
for (i in 1:l){
  przejscie=linki[stri_detect_fixed(tytuly,slownik[i])]
  all[[i]]=przejscie 
}

nowy=vector()
for (n in 1:length(all)){
  w=unlist(all[[n]])
  wynik=as.character(w[which(names(w)=='href')])
  nowy=c(nowy,wynik)
}
nowy=unique(nowy)
nowy=nowy[which(stri_detect_regex(nowy,'autoplay')==FALSE)]
#Rozbijamy na dwa wektory, poniewaz niektore linki zaczynaja sie od http
#a niektorym trzeba dorobic
nowy1=nowy[stri_detect_regex(nowy,'http')]
nowy2=nowy[which(stri_detect_regex(nowy,'http')==FALSE)]

for (i in 1:length(nowy1)){
  godz=as.character(as.numeric(Sys.time()))
  artykul=html(nowy1[i])
  cast_a=html_nodes(artykul,'p , .black')
  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//tvn//',data,'//',godz,'.txt')
  writeLines(html_text(cast_a),nazwa)
}
for(i in 1:length(nowy2)){
  godz=as.character(as.numeric(Sys.time()))
  sciezka=stri_paste('http://www.tvn24.pl',nowy2[i])
  artykul=html(sciezka)
  cast_a=html_nodes(artykul,'p , .black')
  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//tvn//',data,'//',godz,'.txt')
  writeLines(html_text(cast_a),nazwa)
}

# TVN24 strona poswiecona wyborom 
wybory=html("http://www.tvn24.pl/wybory-prezydenckie-2015,117,m")
cast=html_nodes(wybory,'a')
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//tvn//wybory//',data,'//tresc',godz,'.txt')
writeLines(as(wybory,'character'),nazwa)
tytuly=html_text(cast)
linki=html_attr(cast,'href')
all=list()

for (i in 1:l){
  przejscie=linki[stri_detect_fixed(tytuly,slownik[i])]
  all[[i]]=przejscie 
}

nowy=unlist(all)
nowy=unique(nowy)
nowy=nowy[which(stri_detect_regex(nowy,'autoplay')==FALSE)]
nowy=nowy[which(stri_detect_regex(nowy,'http')==FALSE)]

for (i in 1:length(nowy)){
  godz=as.character(as.numeric(Sys.time()))
  sciezka=stri_paste('http://www.tvn24.pl',nowy[i])
  artykul=html(sciezka)
  cast_a=html_nodes(artykul,'p , .black')
  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//tvn//wybory//',data,'//',godz,'.txt')
  writeLines(html_text(cast_a),nazwa)
}


###################### wp.pl #######################
wp=html('http://www.wp.pl')
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//wp//',data,'//tresc',godz,'.txt')
writeLines(as(wp,'character'),nazwa)
cast= html_nodes(wp,"a")
tytuly=html_text(cast)
linki=html_attrs(cast)
all=list()

for (i in 1:l){
  przejscie=linki[stri_detect_fixed(tytuly,slownik[i])]
  all[[i]]=przejscie 
}

nowy=vector()
for (n in 1:length(all)){
  w=unlist(all[[n]])
  wynik=as.character(w[which(names(w)=='href')])
  nowy=c(nowy,wynik)
}
nowy=unique(nowy)
for (i in 1:length(nowy)){
  godz=as.character(as.numeric(Sys.time()))
  godz2=as.character(as.numeric(Sys.time()))
  artykul=html(nowy[i])
  cast_a=html_nodes(artykul,'.artCont')
  cast_b=html_nodes(artykul,'#intertext1 , .lead')
  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//wp//',data,'//',godz,'.txt')
  nazwa2=stri_paste('D://szkola//R i Big Data//projekt 1//wp//',data,'//',godz2,'.txt')
  writeLines(html_text(cast_a),nazwa)
  writeLines(html_text(cast_b),nazwa2)
}
 

######################## newsweek ##################
news=html("http://www.newsweek.pl")
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//newsweek//',data,'//tresc',godz,'.txt')
writeLines(as(news,'character'),nazwa)

cast <- html_nodes(news, 'a')
tytuly=html_text(cast)
linki=html_attr(cast,'href')
all=list()
for (i in 1:l){
  przejscie=linki[stri_detect_fixed(tytuly,slownik[i])]
  all[[i]]=przejscie 
}

nowy=as.vector(na.omit(unlist(all)))
nowy=unique(nowy)
for (i in 1:length(nowy)){
  godz=as.character(as.numeric(Sys.time()))
  artykul=html(nowy[i])
  cast=html_nodes(artykul,'p')

  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//newsweek//',data,'//',godz,'.txt')
  writeLines(html_text(cast),nazwa)
}


############ TVP INFO ###############

tvp=html("http://www.tvp.info")
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//tvp//',data,'//tresc',godz,'.txt')
writeLines(as(tvp,'character'),nazwa)
cast=html_nodes(tvp,'.title')
tytuly=html_text(cast)
linki=vector()
link=vector()

for (i in 1:length(cast)){
  link[i]=stri_extract_all_regex(as(cast[[i]],"character"),"href=.*.")
  dl=stri_length(unlist(link[i]))
  linki[i]=stri_sub(unlist(link[i]),from=7,to=(dl-7))
}
all=list()
for (i in 1:l){
  przejscie=linki[stri_detect_fixed(tytuly,slownik[i])]
  all[[i]]=przejscie 
}
nowy=as.vector(na.omit(unlist(all)))
nowy=unique(nowy)
nowy1=nowy[stri_detect_regex(nowy,'http')]
# pomijamy nowy1 bo to sa odnosniki na inne strony niz tvp
nowy2=nowy[which(stri_detect_regex(nowy,'http')==FALSE)]


for(i in 1:length(nowy2)){
  godz=as.character(as.numeric(Sys.time()))
  sciezka=stri_paste('http://www.tvp.info',nowy2[i])
  artykul=html(sciezka)
  cast_a=html_nodes(artykul,'.text , .lead')
  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//tvp//',data,'//',godz,'.txt')
  writeLines(html_text(cast_a),nazwa)
}



############### onet.pl #########################


onet=html("http://www.onet.pl/")
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//onet//',data,'//tresc',godz,'.txt')
writeLines(as(onet,'character'),nazwa)

cast=html_nodes(onet,'.title , .boxContent a , .firstItem')
tytuly=html_text(cast)
linki=html_attrs(cast)
all=list()

for (i in 1:l){
  przejscie=linki[stri_detect_fixed(tytuly,slownik[i])]
  all[[i]]=przejscie 
}

nowy=vector()
for (n in 1:length(all)){
  w=unlist(all[[n]])
  wynik=as.character(w[which(names(w)=='href')])
  nowy=c(nowy,wynik)
}
nowy=unique(nowy)
for(i in 1:length(nowy)){
  godz=as.character(as.numeric(Sys.time()))
  sciezka=stri_paste(nowy[i])
  artykul=html(sciezka)
  cast_a=html_nodes(artykul,'p , .black')
  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//onet//',data,'//',godz,'.txt')
  writeLines(html_text(cast_a),nazwa)
}


# onet.pl strona poswiecona wyborom
wyb=html('http://wiadomosci.onet.pl/wybory-prezydenckie/xcnpc')
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//onet//wybory//',data,'//tresc',godz,'.txt')
writeLines(as(onet,'character'),nazwa)

cast=html_nodes(wyb,'.datePublished+ a , .itemTitle')
tytuly=html_text(cast)

linki=html_attr(cast,'href')
all=list()

for (i in 1:l){
  przejscie=linki[stri_detect_fixed(tytuly,slownik[i])]
  all[[i]]=przejscie 
}
nowy=unlist(all)

nowy=unique(nowy)
nowy=as.vector(na.omit(nowy))
for(i in 1:length(nowy)){
  godz=as.character(as.numeric(Sys.time()))
  sciezka=stri_paste(nowy[i])
  artykul=html(sciezka)
  cast_a=html_nodes(artykul,'p , .black')
  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//onet//wybory//',data,'//',godz,'.txt')
  writeLines(html_text(cast_a),nazwa)
}

######################## gazeta.pl ########################

gazeta =html("http://gazeta.pl")
godz=as.character(as.numeric(Sys.time()))
nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//gazeta//',data,'//tresc',godz,'.txt')
writeLines(as(gazeta,'character'),nazwa)

cast=html_nodes(gazeta,'a')
link=html_attr(cast,'href')
tytuly=html_text(cast)
linki=vector()
for (i in 1:length(link)){
  linki[i]=stri_extract_all_regex(link[i],"http://.*.html")
}


all=list()
for (i in 1:l){
  przejscie=linki[stri_detect_fixed(tytuly,slownik[i])]
  all[[i]]=przejscie 
}

nowy=as.vector(na.omit(unlist(all)))
nowy=unique(nowy)
nowy=nowy[stri_detect_fixed(nowy,'komunikaty')==FALSE]
for (i in 1:length(nowy)){
  godz=as.character(as.numeric(Sys.time()))
  artykul=html(nowy[i])
  cast=html_nodes(artykul,'#artykul , #gazeta_article_lead')
  nazwa=stri_paste('D://szkola//R i Big Data//projekt 1//gazeta//',data,'//',godz,'.txt')
  writeLines(html_text(cast),nazwa)
}

