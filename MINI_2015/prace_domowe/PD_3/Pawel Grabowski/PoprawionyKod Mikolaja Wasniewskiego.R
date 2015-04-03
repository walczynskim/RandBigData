library(dplyr)
library(stringi)
library(rvest)

## ------------- ONET-------------------------
OnetArtykulInfo <- function(link, id) {
   link <- html(link)
   #zrodlo
   zrodlo <- "wiadomosci.onet.pl"   
   #tytul
   tytul <- html_nodes(link, "#mainTitle > h1")[[1]] %>%
      html_text(encoding="UTF-8") %>%
      stri_trim(.)   
   # tagi
   tagi <- html_nodes(link, "#relatedTopics > span")[-1]%>%
      html_text(encoding="UTF-8") %>% 
      stri_trim(.) %>%
      stri_paste(collapse = ",") %>%
      stri_replace_all_regex(",\\.+","")   
   #data
   data <- html_nodes(link, "#articleHeading > meta") %>% 
      html_attr("content") %>%
      stri_replace_all_regex(data, "\\+[0-9]{4}","")   
   #tresc
   tresc1 <- html_nodes(link, "#lead > p") %>%
      html_text(encoding="UTF-8")
   tresc <- html_nodes(link, "#detail > p") %>% 
      html_text()   
   if (length(tresc) == 0) {
      tresci <- html_nodes(link, "#detail > div.interview > p") %>% 
         html_text()
      tresc <- c()
      for (i in seq_along(tresci)) {
         tresc <- paste(tresc, tresci[i])
      }
      tresc <- paste(tresc1, tresc)
   } else {
      tresc <- stri_paste(tresc1, tresc, collapse = " ")
   } 
   
   #liczba komentarzy
   liczba_kom <- html_nodes(link,
                            "#socialTop > div > div.box3_forum > div.forumCommentButton > div.dymek > div.dymek4") %>%
      html_text(liczba_kom)
   
   data.frame("id"=id, "zrodlo" = zrodlo, "data" = data, "tytul" = tytul, "tresc" = tresc,
              "tagi" = tagi, "liczba komentarzy" = liczba_kom)
}
OnetInfo <- function(link, path1, pathczas) {
   atrykuly_linki <- html(link) %>% 
      html_nodes(., "#staticStreamContent > div > a") %>%
      html_attr(artykuly, "href") %>%
      unique(.)
   # Zapisywanie do pliku po uwzglednieniu daty:
   ZapisDoPliku(pathczas, path, df, onet_artykul_info, atrykuly_linki)
   return(invisible(NULL))
}
Tvn24ArtykulInfo <- function(link, id) {
   link <- html(link)
   #zrodlo
   zrodlo <- "tvn24.pl"
   #tytul
   tytul <- html_nodes(link, "article > h1 > span") %>% 
      html_text(encoding="UTF-8")
   if(length(tytul) == 0) {
      tytul <- html_nodes(link, "div.mainContainer > h1") %>% 
         html_text(encoding="UTF-8")
   }
   tytul <- stri_trim(tytul)
   # tagi
   tagi <- html_nodes(link, "div.relatedTopic > ul > li > a") %>%
      html_text(encoding="UTF-8")
   if (length(tagi) > 0) {
      tagi <- stri_trim(tagi) %>% 
         stri_paste(collapse = ",")
   } else {
      tagi <- ""
   }
   #data
   data <- html_nodes(link, "div.articleDateContainer.borderGreyBottom > time") %>%
      html_attr("datetime")
   #tresci:
   tresc1 <- html_nodes(link, " article > h2.size18.mt10.mb15") %>%
      html_text(encoding="UTF-8") %>%
      stri_trim()
   tresc2 <- html_nodes(link, "div > div.articleDetailHolder > article > p") %>%
      html_text() %>%
      '['(-length(tresc2)]) %>% 
      stri_trim() %>%
      stri_replace_all_regex("(if(\\n|.)+\\}|\\n|\\r|\\t)", " ")
   tresc <- stri_paste(tresc1, tresc2, collapse = " ")
   #liczba komentarzy
   liczba_kom <- html_nodes(link, "#forum > div.headerBgGrey.mb15 > h1 > span") %>%
      html_text(.) %>% 
      stri_extract_all_regex("[0-9]+") %>%
      unlist()
   #zwracana ramka danych
   data.frame("id"=id, "zrodlo" = zrodlo, "data" = data, "tytul" = tytul, "tresc" = tresc,
           "tagi" = tagi, "liczba komentarzy" = liczba_kom)   
}

Tvn24Info <- function(link, path1, pathczas) {
   #linki do artykulow:
   html_link <- html(link)
   artykuly_linki1 <- html_nodes(html_link, "div.textRight > h1 > a") %>%
      html_attr("href")
   artykuly_linki2 <- html_nodes(html_link, "div.textLeft > h1 > a") %>%
      html_attr("href")
   artykuly_linki3 <- html_nodes(html_link, "article.singleArtMain > div > h1 > a") %>%
      html_attr("href")
   artykuly_linki <- stri_paste("http://www.tvn24.pl",
                              c(artykuly_linki1, artykuly_linki2, artykuly_linki3)) %>%
      unique(.)
   ZapisDoPliku(pathczas, path, df, tvn24_artykul_info, atrykuly_linki)
   return(invisible(NULL))
}
WpArtykulInfo <- function(link, id) {
   link <- html(link)
   #data
   data <- html_text(html_nodes(link, "time")[[1]], encoding="UTF-8")
   datas <- unlist(stri_extract_all_regex(data, "[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}"))
   datas <- stri_replace_all_regex(datas, "\\.","-")
   times <- unlist(stri_extract_all_regex(data, "[0-9]{2}:[0-9]{2}"))
   data <- stri_paste(datas, times,sep=" ")
   #zrodlo
   zrodlo <- "wiadomosci.wp.pl"
   #tytul
   tytul <- html_text(html_nodes(link, ".fullWidth .h1")[[1]], encoding="UTF-8")
   # tagi
   tagi <- html_text(html_nodes(link, ".fullWidth .ST-BX-Tagi-dol a"), encoding="UTF-8") %>%
      stri_paste(tagi, collapse = "-")
   #tresc
   tresc1 <- html_nodes(link, ".lead")[[1]] %>%
      html_text(encoding="UTF-8")
   tresc <- html_nodes(link, "section.artCnt:nth-child(1) > main:nth-child(3) > div:nth-child(4)") %>%
      stri_replace_all_regex(html_text(tresc, encoding="UTF-8"), "(if(\\n|.)+\\}|\\n|\\r|\\t)", "") %>%
      stri_paste(tresc1, tresc)
   #liczba komentarzy
   liczba_kom <- html_text(html_nodes(link, "#stgOpinie .opStron+ .opNNav .opAllCom"))
   if (length(liczba_kom) == 0) {
      liczba_kom <- html_text(html_nodes(link, "#opOpinie+ .opNNav .opAllCom"))
   }
   liczba_kom <- unlist(stri_extract_all_regex(liczba_kom, "[0-9]+"))
   data.frame("id"=id, "zrodlo" = zrodlo, "data" = data, "tytul" = tytul, "tresc" = tresc,
              "tagi" = tagi, "liczba komentarzy" = liczba_kom)  
}
WpInfo <- function(link, path1, pathczas) {
   atrykuly_linki <- html(link) %>%
      html_nodes(., ".kontener h2 a") %>%
      stri_paste("http://wiadomosci.wp.pl", html_attr(., "href")) %>%
      unique(.)
   ZapisDoPliku(pathczas, path, df, wp_artykul_info, atrykuly_linki)
   return(invisible(NULL))
}
ZapisDoPliku <- function(pathczas, path, df, strona_artykul_info, atrykuly_linki) {
   czass <- readLines(pathczas)
   czas1 <- strptime(czass, "%Y-%m-%d %H:%M")
   n <- length(atrykuly_linki)
   df <- data.frame()
   for(i in 1:n) {
      dfTemp <- strona_artykul_info(atrykuly_linki[i], i)
      if(unclass(czas1-strptime(dfTemp$data, "%Y-%m-%d %H:%M")) <= 0) {
         df <- rbind(df, dfTemp)
      }
   }
   odroznik <- Sys.time()#ta czesc nazwy pliku odroznia go od pozostalych
   odroznik1 <- strftime(odroznik, "%Y-%m-%d %H:%M:%S %Z")
   writeLines(odroznik1, pathczas)
   path <- stri_paste(path1, "\\a",coll="")
   odroznik <- stri_replace_all_regex(odroznik, "\\.","")
   path <- stri_paste(path, odroznik, coll="") %>%
      stri_paste(path, ".txt",coll="")
   if(nrow(df) > 0) {
      write.table(df, path)  
   }
   return(invisible(NULL))
}
