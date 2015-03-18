library(stringi)
library(rvest)
library(tm)
library(wordcloud)

#linki do książek
ania1 <- "http://www.gutenberg.org/cache/epub/45/pg45.txt"
ania2 <- "http://www.gutenberg.org/cache/epub/47/pg47.txt"
ania3 <- "http://www.gutenberg.org/cache/epub/51/pg51.txt"
ania4 <- "http://gutenberg.net.au/ebooks01/0100251h.html"
ania5 <- "http://www.gutenberg.org/cache/epub/544/pg544.txt"

url <- c(ania1, ania2, ania3, ania4, ania5)

#funkcję utworzyłam dopiero po wstępnej analizie tekstu
podstawoweDane <- function(x){
   #wyciągam tresc
   tresc <- html_text(html(x))
   #przygotowanie tekstu do analizy
   #-tworzenie korpusu
   #-usuwanie białych spacji
   #-usuwanie znaków przystankowych
   #-transformacja na małe litery
   #-usunięcie stopwords
   corpus <- Corpus(VectorSource(tresc))
   corpus <- tm_map(corpus, stripWhitespace)
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, content_transformer(tolower))
   corpus <- tm_map(corpus, removeWords, stopwords("english"))
   #dodatkowo usuwam jeszcze kilka innych słów (wcześniejsza analiza tekstu)
   corpus <- tm_map(corpus, removeWords, 
                    c("say", "said", "mr", "mrs", "miss", "will", "one", "oh", "us"))
   corpus <- tm_map(corpus, stemDocument)  
   
   #wracam do formatu tekstu
   tresc <- unlist(sapply(corpus, `[`, "content"))
   slowa <- stri_extract_all_words(tresc)
   tabelaSort <- sort(table(slowa), dec=TRUE)
   #zamieniam "ann" na "anne" (bo ucięło)
   indAnn <- which(names(tabelaSort)=="ann")
   names(tabelaSort)[indAnn]="anne"
   #ile razy wystąpiło słowo Anne, Gilbert a ile Diana
   anne <- tabelaSort["anne"]
   gilbert <- tabelaSort["gilbert"]
   diana <- tabelaSort["diana"]
   iloscSlow <- sum(tabelaSort)
   return(list(tabelaSort, tabelaSort[1:10], anne, gilbert/iloscSlow,
               diana/iloscSlow))
}

rysuj <- function(tabelaSort){
   pal2 <- brewer.pal(8,"Dark2")
   wordcloud(names(tabelaSort), tabelaSort, min.freq = 70, colors=pal2)
}


listaDanych <- list()
for(i in 1:5){
   listaDanych[[i]] <- podstawoweDane(url[i])
}

for (i in 1:5){
   rysuj(listaDanych[[i]][[1]])
}

dianaCzasowo <- numeric()
dianaCzasowo <- sapply(listaDanych, function(x){
   x[[5]]
})

gilbertCzasowo <- numeric()
gilbertCzasowo <- sapply(listaDanych, function(x){
   x[[4]]
})

plot(dianaCzasowo, type="l", main="Przyjaźń a miłość", col="red", lwd=3)
lines(gilbertCzasowo, type="l", col="green", lwd=3)
legend("topright", legend=c("Diana", "Gilbert"), lty=1, lwd=3, col=c("red", "green"), bty="n")
