require(rvest)
require(stringi)
require(tm)

kandydat <- c(
   "(Komorowski(\\p{L})*)|((B|b)ronkobus(\\p{L})*)",
   "Marian(\\p{L})* Kowalsk(\\p{L})*",
   "(Dud(\\p{L})*)|((D|d)udabus(\\p{L})*)",
   "Paliko(\\p{L})*",
   "Jarubas(\\p{L})*",
   "OgÃ³rek",
   "Korwin(\\p{L})*",
   "Ann(\\p{L})+ Grodzk(\\p{L})*",
   "Jac(e)*(\\p{L})* Wilk(\\p{L})*",
   "Grzegorz(\\p{L})* Braun(\\p{L})*",
   "Kukiz(\\p{L})*"
)

przeszukaj_tvn24 <- function(kandydat=kandydat){
      #kandydat wektor z regexami dla kandydatow
      tvn24 <- html("http://tvn24.pl")      
      
      ### Wybieranie tematÃ³w i zapisywanie ich do ramki danych df ###
      
      ### NEWSY
      status <- "newsy"
      czcionka <- 10
      naglowek <- html_text(html_nodes(tvn24,"#newestNewsList a"),encoding="UTF-8")
      link <- html_attr(html_nodes(tvn24,"#newestNewsList a"),"href")
      df <- data.frame( status, czcionka, naglowek, link )
      
      ### MAINTOPIC
      status <- "maintopic"
      naglowek <- html_text(html_nodes(tvn24,".myCenter"),encoding="UTF-8")
      linki <-  html_attr(html_nodes(tvn24,".myCenter a"),"href")
      czcionka <- html_attr(html_nodes(tvn24,".myCenter a"),"class")
      df <- rbind( df, data.frame( status, czcionka, naglowek, link ) )
      
      ### Obrazki, tu bedzie wiecej klopotow, wszystko po to, zeby artykuly sie nie dublowaly 
      status <- "obrazki"
      naglowek<- html_text(html_nodes(tvn24,".mainLeftColumn  h1 a"),encoding="UTF-8")
      o<-!stri_detect_regex(naglowek,"»")
      o2<-stri_detect_regex(naglowek,".+")
      naglowek<-naglowek[o==o2]
      linki <- html_attr(html_nodes(tvn24,".mainLeftColumn  h1 a"),"href")
      link<-linki[o==o2]
      czcionka <- 0
      df_temp <- data.frame( status, czcionka, naglowek, link )
      df_temp <-df_temp[!duplicated(df_temp),]
      df <- rbind( df, df_temp )

      ### WYSZUKANIE ROZMIARÃ“W TYTUÅÃ“W ###
      # nagÅ‚Ã³wki po rozmiarach
      fonts <- seq(from=10, to=70, by=2)
      size <- vector("list",length(fonts))
      df_size <- data.frame(size=0,tytul="0")
      for( i in seq_along(size) ){
            t<-html_text(html_nodes( tvn24, stri_paste(".size",fonts[i]) ))
            if (length(t))
                  df_size <- rbind(df_size, data.frame(size=fonts[i], tytul = t))
      }
      for( i in seq_along(df_size[,1]) )
            df$czcionka[ which(df$naglowek == as.character(df_size$tytul[i]))] <- df_size$size[i]

      # uwaga! przy newsach powinna byc domyslna czcionka = 10.
      # jesli tak nie jest, mozaliwe, ze ten sam artykul pojawil sie w innym miejscu na stronie
      # w wiekszym formacie. ta pozorna niescislosc moze byc cenna informacja w przysci
      
      
      #zapisywanie artyków dla konkretnego kandydata do oddzielnych plikow:
      for (i in seq_along(kandydat)){
            regex_kandydata <- kandydat[i]
            #sprawdzanie czy nazwisko kandydata wystapi³o w tytule:
            df_kandydata <- df[(stri_detect_regex( df$naglowek, regex_kandydata)), ]
            if (nrow(df_kandydata))
            {
                  linki_do_artykulu <- df_kandydata$link
                  # czyszczenie tytulu
                  tytuly <- stri_replace_all_regex(df_kandydata$naglowek,"\n","")
                  tytuly <- stri_replace_all_regex(tytuly,"\\n|\\t","")
                  #Wyciagniecie treœci artykulow
                  artykuly <- sapply(linki_do_artykulu, function(x){
                        if( stri_detect_regex(x,"http") ){
                           xx <- html_text(html_nodes(html(as.character(x)),"div article"),encoding="UTF-8")[1]
                        } else {
                           xx <- html_text(html_nodes(html(stri_paste("http://tvn24.pl",x)),"div article"),encoding="UTF-8")[1]

                        }
                        if( length(xx) > 0 ){
                                    # wstepne oczyszczenie
                                    corpus <- Corpus(VectorSource(x))                       
                                    corpus <- tm_map(corpus, stripWhitespace)                          #-usuwanie biaÅ‚ych spacji
                                    corpus <- tm_map(corpus, removePunctuation)                        #-usuwanie znakÃ³w przystankowych
                                    corpus <- tm_map(corpus, content_transformer(tolower))             #-transformacja na maÅ‚e litery
                                    unlist(sapply(corpus, `[`, "content"))
                              }else{"brak artykulu"}
                        
                  }) 
                  dff <- unique(data.frame("status" = df_kandydata$status,
                                           "czcionka" = df_kandydata$czcionka,
                                           "tytul" = tytuly,
                                           "artykul" = artykuly,
                                           stringsAsFactors = FALSE ))
                  data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
                  nazwa <- stri_paste(data,"_tvn24_",names(regex_kandydata),".txt", sep="")
                  #zapisuje do pliku txt
                  #write.table(dff,nazwa, row.names=FALSE)
                  cat("Znaleziono",length(unique(artykuly)), "artykul(y/ow) dla kandydata", names(regex_kandydata), ".\n")
            }else NULL
      }
}
przeszukaj_tvn24()


skanuj_natemat_glowna<-function(kandydat=kandzdat){
   #kandydat wektor z regexami dla kandydatow
   urlTematGlowna <- "http://natemat.pl/"
   htmlTematGlowna <- html(urlTematGlowna)
   nodes <- html_nodes(htmlTematGlowna, ".without-body-header .hp-content-image+ h2 a")
   
   #wyciagam linki do artykulow
   urlArt <- html_attr(nodes, "href")
   #indykator czy artykul‚ jest z natemat czy nie
   indNaTemat <- stri_detect_regex(urlArt, ".+natemat.+")
   #biore tylko te, ktore sa z natemat (bo chce zeby artykol mia³ taka sama strukture)
   urlArt <- urlArt[indNaTemat]
   
   #wyciagam tytul
   tytul <- html_text(nodes)[indNaTemat]
   
   # indykator czy artykul dotyczy wyborów prezydenckich 
   czyWybory<-unname(sapply(tytul,function(tyt){
      any(stri_detect_regex(tyt,kandydat))
   ))
   
   #uaktualniam liste artykolow (artykuly dotyczace wyborów prezydenckich)
   urlArt <- urlArt[czyWybory]
   tytul <- tytul[czyWybory]
   
   #wyciagniecie daty i tresc artykolow
   info<-unlist(lapply(urlArt, function(x){
      nodesData <- html_nodes(html(x), ".date")
      dataa <- html_attr(nodesData, "title")
      dataa <- stri_replace_all_fixed(dataa, "T", " ")
      dataa <- stri_replace_all_regex(dataa, "[+-][0-9]{2}:[0-9]{2}", "")
      nodesTresc <- html_nodes(html(x), ".article-body")
      art <- html_text(nodesTresc)
      c(dataa,art)
   }))
   n<-length(info)
   data<-info[seq(1,n-1,2)]
   tresc<-info[seq(2,n,2)]
   
   #dokonuje wstepnego oczyszczenia 
   #-usuwanie bialych spacji
   #-usuwanie znakow przystankowych
   #-transformacja na male litery
   corpus <- Corpus(VectorSource(tresc))
   corpus <- tm_map(corpus, stripWhitespace)
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, content_transformer(tolower))
   
   #tworze ramke danych 
   dfTematGlowna <- data.frame("tytul"=tytul,
                               "data"=data,
                               "tresc"=unlist(sapply(corpus, `[`, "content")),
                               "url"=urlArt,
                               stringsAsFactors=FALSE)
   rownames(dfTematGlowna) <- NULL
   
   #nazwa unikalna pliku do zapisania danych
   nazwa <- paste(as.character(Sys.Date()), "_NaTematGlowna.txt", sep="")
   
   #zapisuje do pliku txt
   #write.table(dfTematGlowna, nazwa, row.names=FALSE)
}

skanuj_natemat_glowna()