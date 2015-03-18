# Dokonuje analizy serialu Chirurdzy oraz korzystajac z tego samego skryptu
# analizy serialu Lekarze. Bede korzystac z informacji zawartych
# na stronie filmweb.pl


############## Chirurdzy ####################################
film=html('http://www.filmweb.pl/serial/Chirurdzy-2005-165277#')

# W pierwszej kolejnosci chce wyciagnac informacje na temat sredniej serialu.
cast=html_nodes(film,'.light span')
srednia=html_text(cast)
srednia # serial otrzymal srednia 7.9

#teraz wydobywam informacje o gatunku serialu
cc=html_nodes(film,'.bottom-15 tr+ tr td')
gatunek=html_text(cc)
gatunek

# teraz bede wyciagac informacjena temat obsady serialu:
obs=html_nodes(film,'.vertical-align a')
obsada=stri_paste('http://www.filmweb.pl',as.character(html_attrs(obs)[[1]]))

str=html(obsada)
aa=html_nodes(str,'.filmCast a')
wyn=html_attrs(aa)
l=length(wyn)
linki=vector(length=l)
for (i in 1:l){
  
  linki[i]=stri_paste('http://www.filmweb.pl',as.character(wyn[[i]][1]))
}

linki=unique(linki)
jpg=stri_detect_fixed(linki,'jpg')
all=linki[which(jpg==FALSE)]
ll=length(all)
srednie=vector(length=ll)
nazwa=vector(length=ll)
for (i in 1:l){
  akt=html(all[i])
  oc=html_nodes(akt,'.vertical-align.light')
  srednie[i]=html_text(oc)[1]
  n=html_nodes(akt,'.s-32.top-5')
  nazwa[i]=html_text(n)
}
srednie # jak widac jedna wartosc jest NA co wynika z faktu ze nikt danemu
# aktorowi nie dal zadnej oceny i nie ma sredniej
nazwa # wszyscy aktorzy
stri_sub(srednie,2,2)='.' #trzeba zamienic przecinki na kropki gdyz potrzebujemy formatu
                          #numerycznego
sr_num=as.numeric(srednie)
nazwa[which.max(sr_num)]# osoba o najwiekszej sredniej
barplot(as.numeric(na.omit(sr_num)))
#jak widac na wykresie wszystkie oceny aktorow sa dosc zblizone

# Ponizej jeszcze przedstawienie wszystkich aktorow i im odpowiadajacych srednich ocen
print(paste(nazwa,sr_num, sep= ' : '))


##################### Lekarze #####################

film=html('http://www.filmweb.pl/serial/Lekarze-2012-635863')
cast=html_nodes(film,'.light span')
srednia=html_text(cast)
srednia

cc=html_nodes(film,'.bottom-15 tr+ tr td')
gatunek=html_text(cc)
gatunek # tu jak widac nie zadzialal ten sam skrypt

obs=html_nodes(film,'.vertical-align a')
obsada=stri_paste('http://www.filmweb.pl',as.character(html_attrs(obs)[[1]]))

str=html(obsada)
aa=html_nodes(str,'.filmCast a')
wyn=html_attrs(aa)
l=length(wyn)
linki=vector(length=l)
for (i in 1:l){
  
  linki[i]=stri_paste('http://www.filmweb.pl',as.character(wyn[[i]][1]))
}

linki=unique(linki)
jpg=stri_detect_fixed(linki,'jpg')
all=linki[which(jpg==FALSE)]
ll=length(all)
srednie=vector(length=ll)
nazwa=vector(length=ll)
for (i in 1:l){
  akt=html(all[i])
  oc=html_nodes(akt,'.vertical-align.light')
  srednie[i]=html_text(oc)[1]
  n=html_nodes(akt,'.s-32.top-5')
  nazwa[i]=html_text(n)
}
srednie 
nazwa 
stri_sub(srednie,2,2)='.' 
sr_num=as.numeric(srednie)
nazwa[which.max(sr_num)]
barplot(as.numeric(na.omit(sr_num)))

print(paste(nazwa,sr_num, sep= ' : '))


# Jak widac skrypt zadzialal dla obydwu serialu prawidlowo,poza jednym miejscem
# opisujacym gatunek serialu.