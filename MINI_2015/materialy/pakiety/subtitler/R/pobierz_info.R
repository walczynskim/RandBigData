#' Funkcja pobiera informacje ze strony IMDb oraz Wikipedii o danym filmie, oraz pobiera napisy do niego.
#' 
#' @param id, id filmu (zgodnie z IMDb)
#'
#' @examples 
#' pobierz.info.just.jankowiak(0499549) #Avatar
#' pobierz.info.just.jankowiak(0110912) #Pulp Fiction#'
#' pobierz.info.just.jankowiak(0126029) #Titanic
#' pobierz.info.just.jankowiak(0050083) #Dwunastu gniewnych ludzi
#'
#' @import rvest
#' @import stringi
#' @import XML
#' 
#' @author Justyna Jankowiak

pobierz.info.just.jankowiak <- function(id){
   imdb <- info.imdb(id)
   wiki <- info.wiki(imdb$Title, imdb$Year)
   downloadSubtitle(id)
   return(list(imdb=imdb, wiki=wiki))
}

info.imdb <- function(id) {
   
   url.imdb <- paste("http://www.imdb.com/title/tt", id, sep="")
   film <- html(url.imdb)
   
   tytul = wyjmij(film,'.header .itemprop')
   tytul = sprawdz(tytul)
   rok = wyjmij(film,'.header .nobr')
   rok = stri_sub(rok,2,5) # pozbywam sie zbednych nawiasow
   rok = sprawdz(rok)
   czas = wyjmij(film,'time')
   czas = czas[2] # na stronie zawsze sa podane dwa czasy(takie same) wiec biore jeden
   czas = sprawdz(czas)  
   gatunek = wyjmij(film,'.infobar .itemprop')
   gatunek = stri_paste(gatunek,collapse='@')
   gatunek = sprawdz(gatunek)
   data_wydania = wyjmij(film, '.infobar .nobr a')
   data_wydania = sprawdz(data_wydania)
   fabula = wyjmij(film,'#titleStoryLine p')
   fabula = stri_replace_all_regex(fabula,';',',')
   fabula = sprawdz(fabula)
   kraj = wyjmij(film,'.txt-block:nth-child(4) a')
   if (length(kraj)>1) kraj=stri_paste(kraj,collapse='@')
   kraj = sprawdz(kraj)
   dane_oceny = wyjmij(film,'.star-box-details , .star-box-details span')
   ocena = dane_oceny[2]
   ocena = sprawdz(ocena)
   max_ocena = dane_oceny[4]
   max_ocena = sprawdz(max_ocena)
   liczba_glosujacych = dane_oceny[5]
   liczba_glosujacych = sprawdz(liczba_glosujacych)
   liczba_recenzji = dane_oceny[6]
   liczba_recenzji = sprawdz(liczba_recenzji)
   link_and_title = html_nodes(film, "a")
   links = html_attr(link_and_title, name="href")
   titles = html_text(link_and_title)
   wek_pom = stri_detect_regex(titles,'See full cast')
   ktory = which(wek_pom==TRUE)
   w=links[ktory]
   obsada_link = stri_paste(url.imdb,w[2])
   if (length(obsada_link)>0 && !is.na(obsada_link)){
      prod_music = get_names(obsada_link)
      muzyka = prod_music$music
      muzyka = stri_paste(muzyka,collapse='@')
      muzyka = sprawdz(muzyka)
      producenci = prod_music$producers
      producenci = stri_paste(producenci,collapse='@')
      producenci = sprawdz(producenci) 
   } else{
      muzyka = "NA"
      producenci = "NA"
   }
   ktory = stri_locate_all_regex(html_text(film),'Budget')
   ktory = unlist(ktory)
   budzet = stri_sub(html_text(film),ktory[1]+7,ktory[2]+28)
   budzet = stri_replace_all_regex(budzet,'\\p{WHITE_SPACE}',' ')
   if (length(ktory)>2){
      budzet = 'NA'
   }
   budzet = sprawdz(budzet)
   klucze = wydobadz(links,titles,film,'Keywords','.sodatext') 
   klucze = stri_trim(klucze,'both')
   klucze = stri_paste(klucze,collapse='@')
   klucze = sprawdz(klucze)
   id_film = stri_sub(url.imdb,27,35)
   id_film = sprawdz(id_film)
   lista.imdb = list(
                 id = id_film,
                 link = url.imdb,
                 Title = tytul,
                 Year = rok,
                 Time = czas,
                 Relase = data_wydania,
                 Genre = gatunek,
                 Storyline = fabula,
                 Country = kraj,
                 Ratings = ocena,
                 Ratings_max = max_ocena,
                 Users = liczba_glosujacych, 
                 Reviews_number = liczba_recenzji,
                 Budget = budzet,
                 Keywords = klucze,
                 Music = muzyka,
                 Produced = producenci)

   return(as.data.frame(lista.imdb, stringsAsFactors = FALSE))
}

wyjmij = function(film, nodes){
   wez = html_nodes(film,nodes)
   nazwa = html_text(wez)
   nazwa = stri_replace_all_regex(nazwa,'\\p{WHITE_SPACE}',' ')
   if (length(nazwa)==0){
      return("NA")
   } else {
      return(nazwa)
   }
}

sprawdz = function(cecha){
   if (is.na(cecha)){
      cecha='NA'  
   }else return(cecha)
}

wydobadz = function(links,titles,film,nodes1,nodes2){
   wek_pom = stri_detect_regex(titles,nodes1)
   ktory = which(wek_pom==TRUE)
   wynik=vector()
   if (length(ktory) > 0 ){
      w=links[wek_pom]
      link_n = stri_paste('http://www.imdb.com',w[1])
      awards = html(link_n)
      awards = html_nodes(awards,nodes2)
      wynik = html_text(awards)
      wynik = stri_replace_all_regex(wynik,'\\p{WHITE_SPACE}',' ')  
   }
   if (length(wynik)==0){
      return("NA")
   } else {
      return(wynik)
   }
}

get_names <- function(www) {
   url <- tryCatch({      
      html(www)   
   }, error = function(e) {"NA"})
   if(is.character(url)&&url=="NA") return("NA")
   prod <- tryCatch({
      url %>% html_nodes(".simpleCreditsTable:nth-child(9) a") %>% html_text() %>% 
         stri_trim_both()
   }, error = function(e) {
      "NA"
   })
   music <- tryCatch({
      url %>% html_nodes(".simpleCreditsTable:nth-child(11) a") %>% html_text() %>% 
         stri_trim_both()
   }, error = function(e) {
      "NA"
   })
   if (length(prod) == 0) 
      prod <- "NA"
   if (length(music) == 0) 
      music <- "NA"
   return(list(music = music, producers = prod))
} 

info.wiki <- function(tytul, rok) {
   slowa <- unlist(stri_extract_all_words(tytul))
   tytul <- paste(slowa, collapse="_")
   url <- paste("http://pl.wikipedia.org/wiki/", tytul, sep="")
   html <- html(url)
   tabela <- html_nodes(html, ".infobox td")
   if(length(tabela) == 0) {
      url <- paste("http://pl.wikipedia.org/wiki/", tytul, "_(film_", rok, ")", sep="")
      html <- html(url)
      tabela <- html_nodes(html, ".infobox td")
   }
   
   tabela_tekst <- html_text(tabela)[-1]
   jakie_info <- c("Gatunek", "Kraj produkcji", "J\u0119zyk",
                  "Czas trwania", "Re\u017Cyseria", "Bud\u017Cet")
   w <- match(jakie_info, tabela_tekst)
   info <- data.frame(gatunek = character(0), 
                      kraj_produkcji = character(0), jezyk = character(0),
                      czas_trwania = character(0), rezyseria = character(0),
                      budzet = character(0), stringsAsFactors = FALSE)
   info <- rbind(info, data.frame(gatunek = ifelse(is.na(w[1]), NA, tabela_tekst[w[1]+1]),
                                  kraj_produkcji = ifelse(is.na(w[2]), NA, tabela_tekst[w[2]+1]),
                                  jezyk = ifelse(is.na(w[3]), NA, tabela_tekst[w[3]+1]),
                                  czas_trwania = ifelse(is.na(w[4]), NA, tabela_tekst[w[4]+1]),
                                  rezyseria = ifelse(is.na(w[5]), NA, tabela_tekst[w[5]+1]),
                                  budzet = ifelse(is.na(w[6]), NA, tabela_tekst[w[6]+1]),
                                  stringsAsFactors = FALSE))
   return(t(info))
}


downloadSubtitle <- function(id, unzip = TRUE, info = FALSE, lan="eng") {
   if (!is.logical(unzip) | !is.logical(info))
      stop("arguments 'unzip' and 'info' must be logical")
   if (info == TRUE & unzip == FALSE)
      stop("'info' can be TRUE only if 'unzip' is TRUE")
   
   url <- paste("http://www.opensubtitles.org/en/search/sublanguageid-", lan, "/subadddate-3/imdbid-", id, sep="")
   html <- html(url)
   
   nodes <- html_nodes(html, "#search_results td:nth-child(5) a")
   href <- html_attr(nodes, "href")[1]
   hrefShort <- unlist(stri_extract_all_regex(href, "[0-9]+"))
   hrefBig <- paste("http://www.opensubtitles.org", href, sep = "")
   
   name <- "downloaded_subtitles"
   dir <- file.path(getwd(), name)
   if (!file.exists(dir))
      dir.create(file.path(getwd(), name))
   nameWithDir <- paste(name, "/", hrefShort, ".zip", sep = "")
   download.file(hrefBig, nameWithDir, mode = "wb")
   if (unzip) {
      con <- unzip(nameWithDir, exdir = file.path(getwd(), name))
      file.remove(nameWithDir)
      if (!info) {
         file.remove(con[2])
      }
   }
   return("Downloaded.")
}