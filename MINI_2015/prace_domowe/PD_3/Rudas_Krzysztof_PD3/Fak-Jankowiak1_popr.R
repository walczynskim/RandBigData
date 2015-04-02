# biblioteki

library(rvest)
library(stringi)
library(tm)

# link strony
urlStrona <- "http://wpolityce.pl/"
html<-html(urlStrona)

#pobiera tytuły, date, url do odpowiedniego typu artykułu

#css - część css (ta która rozróżnia wielkość artykułu)
#typ - napis, który chcemy dodać przy danem typie artykułu, np. "top", "duże"...
pobierz <- function(css, typ){
   tytul <- html_text(html_nodes(html, paste(css, " a h3", sep="")))
   data <- as.POSIXct(html_text(html_nodes(html, paste(css, " .js-relative-date", sep=""))))
   url <- html_attr(html_nodes(html, paste(css, " a", sep="")), "href")
   url <- paste(urlStrona, stri_sub(url, 2), sep="")
   data.frame("tytul"=tytul, "data"=data, "url"=url, "waznosc"=typ,
                     stringsAsFactors=FALSE)
}

cssTop <- ".single-headline-3col"
cssTop2 <- ".double-headline-3col"
cssDuze <- ".article-with-wide-thumb-2col"
cssSrednie <- ".article-with-narrow-thumb-2col"
cssMale <- ".article-with-thumb-1col"
cssMini <- ".box-x-3 .article-1col"

#wlozmy wszystko do jednej ramki danych
dodfAll<-function(df,cssvec,typ)
{
   for(i in 1:length(typ))
   {
         df <- rbind(df, pobierz(cssvec[i], typ[i])) 
   }
   df
}
df<-data.frame()
dfAll<-dodfAll(df,c(cssTop,cssTop2,cssDuze,cssSrednie,cssMale,cssMini),
        c("top","top2","duze","srednie","male","mini"))

#mam ramkę danych ze wszystkimi artykułami wraz ze znacznikiem wielkości artykułu
#teraz wyeliminiuję te artykuły, które nie dotyczą żadnego z kandydatów

#Slownik
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

n <- length(dfAll$tytul)

#petla sluzy wyznaczeniu ktore artykuly zawieraja informacje o kandydatach
czyWybory <- logical()
for (i in 1:n){
 
   czyWybory[i] <- any(stri_detect_regex(dfAll$tytul[i],kandydat))
}
#uaktualniam ramkę
dfAll <- dfAll[czyWybory,]

#teraz muszę jeszcze powyciągać treści wybranych już artykułów
#wyciagam treść
tresc <- sapply(dfAll$url, function(x){
   nodes <- html_nodes(html(x), ".bbtext")
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

#dodaje kolumne z trescia
dfAll$tresc <- unlist(sapply(corpus, `[`, "content"))

#nazwa z datą pobierania danych
data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
nazwa <- paste(as.character(data), "_wPolityceGlowna.txt", sep="")

#zapisuje do pliku csv
write.csv(dfAll, nazwa, row.names=FALSE)
