#Wczytywanie pakietów

library(rvest)
library(stringi)
library(tm)

#Link do rozpatrywanej strony

htmlOnetGlowna <- html("http://www.onet.pl/")

pobierz<-function(link,selectorWord)
{
   nodes <- html_nodes(link, selectorWord)
   urlArt <- html_attr(nodes, "href")
   tytul <- stri_trim_both(html_text(nodes))
   c(urlArt,tytul)
}
#Wyciagniecie tresci i linku do artykulu, ktory jest wiadomoscia glowna na stronie
Best<-pobierz(htmlOnetGlowna,".bestOfOnetTop")
#Wyciagniecie tresci i linku do artykulow, bedacych pod wiadomoscia glowna
Top<-pobierz(htmlOnetGlowna,".sliderItem")
#Wyciągnięcie linkow i tytulow, z glownej z zakladki wiadomosci
Art<-pobierz(htmlOnetGlowna,"#boxNews>article>section>div>ul>li>a")
lB<-length(Best)
lT<-length(Top)
lA<-length(Art)
#wszystkie tytuly z glównej potencjalnie mogące być o wyborach

urlAll <- c(Best[1:(lB/2)], Top[1:(lT/2)], Art[1:(lA/2)])
tytulAll <- c(Best[((lB/2)+1):lB], Top[((lT/2)+1):lT], Art[((lA/2)+1):lA])

#Do kazdego dokladamy wage dla wiadomosci glownej "duze", dla wiadomosci pod glowna
# "srednie", dla znajdujacych sie na glownej z zakladki wiadomosci "male"

waznosc <- c("duze", rep("srednie", lT/2), rep("male", lA/2))


#wyciagam tylko te artykuly, w ktorego tytule pojawiło się nazwisko kandydatów
kandydat <- c(
   "(Komorowski(\\p{L})*)|((B|b)ronkobus(\\p{L})*)",
   "Marian(\\p{L})* Kowalsk(\\p{L})*",
   "(Dud(\\p{L})*)|((D|d)udabus(\\p{L})*)",
   "Paliko(\\p{L})*",
   "Jarubas(\\p{L})*",
   "Ogórek",
   "Korwin(\\p{L})*",
   "Ann(\\p{L})+ Grodzk(\\p{L})*",
   "Jac(e)*(\\p{L})* Wilk(\\p{L})*",
   "Grzegorz(\\p{L})* Braun(\\p{L})*",
   "Kukiz(\\p{L})*"
)

# chcemy wyszukac te artykuly gdzie pojawia sie nazwisko kandydata

n <- length(tytulAll)
czyWybory <- logical()
for (i in 1:n){
   czyWybory[i] <- any(stri_detect_regex(tytulAll[i], kandydat))
}

#uaktualniam listę 
urlAll <- urlAll[czyWybory]
tytulAll <- tytulAll[czyWybory]
waznosc <- waznosc[czyWybory]

#wyciagam treść
tresc <- sapply(urlAll, function(x){
   nodes <- html_nodes(html(x), ".hyphenate")
   art <- paste(html_text(nodes), collapse=" ")
   return(art)
})
names(tresc)<-NULL
#dokonuje wstępnego oczyszczenia 
#-usuwanie białych spacji
#-usuwanie znaków przystankowych
#-transformacja na małe litery
corpus <- Corpus(VectorSource(tresc))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))

#tworze ramke danych 
dfOnetGlowna <- data.frame("tytul"=tytulAll, 
                           "tresc"=unlist(sapply(corpus, `[`, "content")),
                           "url"=urlAll, "waznosc"=waznosc,
                           stringsAsFactors=FALSE)
#nazwa z datą pobierania danych
data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
nazwa <- paste(as.character(data), "_OnetGlowna.txt", sep="")

#zapisuje do pliku csv
if(nrow(dfOnetGlowna!=0)){
   write.csv(dfOnetGlowna, nazwa, row.names=FALSE)
}