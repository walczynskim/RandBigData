   #biblioteki
   require(rvest)
   require(stringi)
   require(tm)
   #link
   html<-html("http://interia.pl/")
   #funkcja do tworzenia ramki danych (status,naglowek,link)
   pobierz<-function(df,link,selectorWord,status)
   {
      naglowek <- html_text(html_nodes(link,selectorWord))
      naglowek<-stri_replace_all_regex(naglowek,"\n","")
      link1 <- html_attr(html_nodes(link,selectorWord),"href")
      df<-rbind(df,data.frame( status, naglowek, link1 ))
      df
   }
   # do ramki danych wkladamy wszystkie istotne informacje z glownej
   df<-data.frame()
   df<-pobierz(df,html,".news-one-a","duzy news")### MAIN TOPIC ###
   df<-pobierz(df,html,".tiles-a","obrazki duze")### OBRAZKI DUŻE ###
   df<-pobierz(df,html,".news-a","newsy") ### NEWSY ###
   ###KANDYDACI
   #Dokladam plik pomocniczy, bo inacze funkcja nie zadziala
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
   names(kandydat) <- c("komorowski","kowalski","duda","palikot","jarubas","ogórek","korwin","grodzka",
                 "wilk","braun","kukiz")
   #Przypisujemy artykuly do kandydatow, ktorzy sie w nich pojawiali
   for( i in seq_along(kandydat)){
      regex_kandydata <- kandydat[i]
      df_kandydata <- df[(stri_detect_regex( df$naglowek, regex_kandydata ) ), ]
      if( nrow(df_kandydata) )
      {
         linki_do_artykulu <- df_kandydata$link
         artykuly <- sapply( linki_do_artykulu, function(x){
            y <- html_text(html_nodes(html(as.character(x)),".main-content p"))
            y <- stri_flatten(y)
            #Dlugosc wektora musi byc wieksza od zera inaczej nie mamy czego czyscic
            if( length(y)> 0){
               # wstępne oczyszczenie
               corpus <- Corpus(VectorSource(y))
               corpus <- tm_map(corpus, stripWhitespace) #-usuwanie białych spacji
               corpus <- tm_map(corpus, removePunctuation) #-usuwanie znaków przystankowych
               corpus <- tm_map(corpus, content_transformer(tolower)) #-transformacja na małe litery
               unlist(sapply(corpus, `[`, "content"))
            }else{"brak artykulu"}    
         })
         #finalowa ramka danych dla konkretnego kandydata
         dff <- unique(data.frame("status" = df_kandydata$status,
                                  "tytul" = df_kandydata$naglowek,
                                  "artykul" = artykuly,
                                  stringsAsFactors = FALSE ))
         data <- strftime(Sys.time(),"%Y-%m-%d@%H-%M")
         nazwa <- paste(data, paste0("_interia_",names(regex_kandydata),".txt"), sep="")
         #zapisuje do pliku txt
         write.csv(dff, file.path("dane","interia",names(regex_kandydata),nazwa), row.names=FALSE)
         #informacja ile artykulow i na temat kogo znaleziono
         cat("Znaleziono",length(unique(artykuly)), "artykul(y/ów) dla kandydata", names(regex_kandydata), ".\n")
      }else NULL
   }

   