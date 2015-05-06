#' Pobieranie informacji o filmie
#'
#' Funkcja \code{film_info} pobiera do folderu:
#' 1. informacje o filmie (tytul, rok, rezyseria, muzyka itd...) (z portalu IMDb)
#' 2. uproszczone statystyki dotyczace filmu: ocena, liczba oddanych glosow (z portalu IMDb)
#' 3. napisy do filmu (ze strony opensubtitles.org)
#' 4. informacje o tym, czy film dostal nagrode
#'
#' @param link zmienna typu character wskazujaca na sciezke do filmu w serwisie IMDb.
#' @return folder z zawierajacy dane o filmie
#' @author opracowala: Katarzyna Fak na podstawie projektow grupy FSS i PRS i wlasnej pracy domowej 06.
#'
#' @examples
#' film_info("http://www.imdb.com/title/tt0118799/")
#' @import rvest
#' @import stringi
#' @import dplyr
#' @import XML
#' @import RCurl
#' @export


film_info <- function( link ){
      if(!file.exists(file.path("film_info")))
            dir.create(file.path("film_info"))
      path <- getwd()
      setwd("film_info")
      # 1. zrodlo: IMDb
      cast_n_crew <- from_full_cast_n_crew(link, sep="|")
      main_page <- from_main_page(link, sep="|")
      page_source <- from_page_source(link, sep="|")
      # 2. zrodlo: opensubtitles
      tytul_filmu <- as.character(main_page$title)
      pobierz_napisy(tytul_filmu,"pol")
      # 3. zrodlo: wikipedia
      wikipedia <- wikipedia_charakterystyki_nagrody()
      wikipedia$tytul <- stri_replace_all_fixed(wikipedia$tytul,c("ą","ć","ę","ł","ń","ó","ś","ź","ż","Ż","Ć","Ł","Ó","Ś","Ź"),
                                                   c("a","c","e","l","n","o","s","z","z","Z","C","L","O","S","Z"),vectorize_all = FALSE)
      wikipedia %>%
            filter(tytul==tytul_filmu) %>%
            select(nagroda) -> nagrody
      # poniewaz zaden tytul w tej liscie nie dostaje obu nagrod (dziwne...)
      nagrody_wikipedia <- list(nagrody=ifelse(nrow(nagrody)==0,"Film nie dostal oskara ani zlotego globu",unlist(nagrody)))
      result <- append(append(main_page,cast_n_crew),append(page_source,nagrody_wikipedia))
      sink("info.txt")
      print(result)
      sink()
      message("\nDone\n")
      setwd(path)
      return(result)
}


### POBIERANIE Z IMBDb ###

from_full_cast_n_crew <- function( link, sep="," ){
      tytuly_kolumn <- c("DirectedBy","Cast","Writing","ProducedBy","MusicBy","CinematographyBy")

      link <- paste0(link, ifelse(stri_sub(link,-1)=="/", "", "/"), "fullcredits")
      tables <- link %>% getURL %>% readHTMLTable          # wczytanie wszystkich tabel z podstrony Cast&Crew
      n <- length(tables)
      headers <- link %>%                       # wczytanie naglowkow tabelek
            html %>% html_nodes("h4") %>% html_text %>% "["(1:n)
      # Zamieniam najistotniejsze nazwy, aby byly uniwersalne dla kazdej
      # pobranej tabelki (czasami dopisuja jakies pierdoly w nawiasach)
      headers[ stri_detect_regex(headers, "Directed") ] <- tytuly_kolumn[1]
      headers[ stri_detect_regex(headers, "Cast[^\\p{L}]") ] <- tytuly_kolumn[2]   # uwaga, zeby Casting By nie zamienilo na Cast!
      headers[ stri_detect_regex(headers, "Writing") ] <- tytuly_kolumn[3]
      headers[ stri_detect_regex(headers, "Produced") ] <- tytuly_kolumn[4]
      headers[ stri_detect_regex(headers, "Music [B|b]y") ] <- tytuly_kolumn[5]
      headers[ stri_detect_regex(headers, "Cinematography") ] <- tytuly_kolumn[6]
      # Nadanie nazw tabelom:
      names(tables) <- headers
      tables$Cast <- tables$Cast[,-1]      # pierwsza kolumna Cast jest pusta, bo jest na zdjecia.
      # Wydobycie *pierwszych* kolumn tabel: interesuja nas tylko nazwiska aktorow, a nie np. ze Eddie Murphy byl glosem Osla.
      info_z_cast_crew <- lapply(tytuly_kolumn, function(h){
            zawartosc_tabelki <- as.character(tables[[h]][,1])
            paste0(zawartosc_tabelki[nchar(zawartosc_tabelki)>1],collapse = sep)
            # nchar>1 dlatego, ze czasem \n bierze jako char dlugosci=1.
      })
      names(info_z_cast_crew) <- tytuly_kolumn
      return(info_z_cast_crew)
}

from_main_page <- function( link, sep="," ){
      # 1. wydlubanie informacji ze strony glownej filmu nodesami:
      all_nodes <- c(title=".header .itemprop",
                     year=".header a",                      #zwraca character => mozna zmienic na numeric
                     duration="#overview-top time",         #zwraca character => mozna zmienic na numeric
                     genres=".infobar .itemprop",
                     rating="div.titlePageSprite",
                     votes=".star-box-details a:nth-child(3) span")
      page <- html(link)
      wydlub <- function(node_name){
            item <- page %>% html_nodes( all_nodes[node_name] ) %>% html_text %>% stri_trim
            if( length(item)>0 ) return(item)
            else return(NA)
      }
      info_z_glownej <- lapply(names(all_nodes),wydlub)
      names(info_z_glownej) <- names(all_nodes)

      # zmiana formatowania czasu trwania filmu.
      if( length(info_z_glownej$duration)>0 )
            info_z_glownej$duration <- unlist(stri_extract_all_regex(info_z_glownej$duration,"[0-9]+"))  #zwraca character/NA

      # zmiana gatunkow na wiele kolumn
      info_z_glownej$genres <- paste0(info_z_glownej$genres,collapse = sep)
      return(info_z_glownej)
}

from_page_source <- function(link, sep=",") {
      page <- readLines(link)
      page <- paste(page, collapse = "")
      znaczniki <- c(production_countries = "(?<=Country:).+?(?=</div)", language = "(?<=Language:).+?(?=</div)", color = "(?<=Color:).+?(?=</a)")
      details <- function(znacznik) {
            item <- unlist(stri_extract_all_regex(page, znacznik))
            if (!is.na(item)) {
                  item <- unlist(stri_extract_all_regex(item, "(?<=itemprop='url'>)([a-zA-Z]| )+"))
                  paste0(item, collapse = sep)
            } else item <- NA
      }
      a <- sapply(znaczniki, details)
      names(a) <- names(znaczniki)
      return(a)
}


### POBIERANIE Z SUBTITLES ###
pobierz_napisy <- function(tytul, jezyk){
      url <- znajdz_tytul_jezyk(tytul, jezyk)
      # wczytanie wyszukanych na stronie url linkow i tytulow filmow. Funkcje wykonamy, jesli jakiekolwiek
      # sie znalazly.
      hrefs_titles <- linki_i_tytuly_na_stronie(url)
      if( length(hrefs_titles)>0 ){
            ktory_tytul <- hrefs_titles["title",]
            # Co jesli uzytkownik podal film "Shrek" a program nie wie, czy chodzilo mu o
            # Shrek - Shrek, czy "Shrek Forever" czy "Shrek 2"?

            # lub co, jesli uzytkownik chcialby obejrzec Star Wars'y ale nie jest pewny, jakie napisy sa
            # dostepne lub jak brzmi pelny tytul?

            # jesli wszystkie wczytane tytuly filmow sa takie same program automatycznie pobierze pierwsze
            # z brzegu.
            if( !all(duplicated(ktory_tytul)[-1]) ){
                  print(ktory_tytul)
                  cat("Ktorych napisow potrzebujesz?")
                  ktory_film <- as.numeric(scan(file = "", what = "", nmax = 1))
            }else ktory_film <- 1

            url_napisow <- paste0("http://opensubtitles.org", hrefs_titles["href", ktory_film])
            pobierz_attrs <- atrybuty_pobierz(url_napisow)
            if( !is.null(pobierz_attrs) ){
                  pobierz_rozpakuj(pobierz_attrs)
                  # Normalnie napisy powinny pobrac sie juz w tym kroku, jednak czesto na stronie
                  # opensubtitles.org trzeba kliknac w jeszcze jeden link np. dla filmow
                  # "La vita e bella" lub "Titanic". Jezeli atrybuty pobierania w tym kroku
                  # ustawione sa na NULL (pomimo, ze stri_detektor wykryl film) nalezy wykonac
                  # jeszcze jedno przejscie wglab strony.
            }else{
                  hrefs_titles_zagniezdzone <- linki_i_tytuly_na_stronie(url_napisow)
                  if(length(hrefs_titles_zagniezdzone)==0)
                        stop("Niedokladne zapytanie, mozliwe, ze szukasz serialu?")
                  # czy_znaleziono_tytul <- stri_detect_regex(tolower(href_n_title[2,]), tolower(tytul))
                  # nie trzeba sprawdzac, czy znaleziono tytul, bo na to juz wskazuje sam fakt ze sie wyszukalo.
                  zip_url <- paste0("http://opensubtitles.org",hrefs_titles_zagniezdzone["href", 1]  )
                  pobierz_attrs2 <- atrybuty_pobierz(zip_url)
                  if( !is.null(pobierz_attrs2) ){
                        pobierz_rozpakuj(pobierz_attrs2)
                  }else stop("Nie moge dogrzebac sie do Twoich napisow... ;/")
            }

      }else{
            stop("Nie ma napisow dla tego filmu lub w tym jezyku ;(")
      }
}

linki_i_tytuly_na_stronie <- function(url){
      dostepne_napisy <- html_nodes(html(url),".bnone")
      attrs <- html_attrs(dostepne_napisy)
      hrefs_titles <- sapply(attrs, function(x) x[c("href","title")])
      return(hrefs_titles)
}

atrybuty_pobierz <- function(url_napisow){
      pobierz_button <- html_nodes(html(url_napisow),"#bt-dwl")
      attrs2 <- html_attrs(pobierz_button)
      pobierz <- unlist(attrs2)
      return(pobierz)
}

pobierz_rozpakuj <- function(pobierz_attrs){
      dir_name <- pobierz_attrs["data-installer-file-name"]
      tmp <- tempfile()
      download.file(pobierz_attrs["rel"],tmp)
      if( dir_name%in%dir() )
            message("Uwaga, nadpisanie istniejacych plikow!") else
                  dir.create(dir_name)
      unzip(tmp, exdir=dir_name)
      unlink(tmp)
      message("Napisy pobrano, szefie.")
}

### POBIERANIE Z WIKIPEDII ###
wikipedia_charakterystyki_nagrody <- function(nagroda = c("oscar", "zloty glob")){
      nagroda <- unique(nagroda)
      w_oscar <- "oscar" %in% nagroda
      w_zloty <- "zloty glob" %in% nagroda
      wynik <- data.frame(tytul = character(0), nagroda = character(0), stringsAsFactors = FALSE)
      if(!w_oscar & !w_zloty){return(wynik)}
      linki <- c("http://pl.wikipedia.org/wiki/Kategoria:Filmy_nominowane_do_Oscara",
                 "http://pl.wikipedia.org/w/index.php?title=Kategoria:Filmy_nominowane_do_Oscara&pagefrom=Wi%C4%99zie%C5%84+kr%C3%B3lewski#mw-pages",
                 "http://pl.wikipedia.org/wiki/Kategoria:Filmy_nagrodzone_Z%C5%82otym_Globem_dla_najlepszego_filmu_dramatycznego")
      nodes <- c(".mw-category-group a", ".mw-category-group a", ".mw-category-group+ .mw-category-group li")

      if(w_oscar & !w_zloty){
            linki <- linki[1:2]
            nodes <- nodes[1:2]
      }
      if(!w_oscar & w_zloty){
            linki <- linki[3]
            nodes <- nodes[1]
      }

      for(i in 1:length(linki)){
            strona <- html(linki[i])
            tytuly <- html_nodes(strona, nodes[i])
            tytul <- html_text(tytuly)
            if(stri_detect_fixed(linki[i], "Oscar")){
                  nag <- rep("oscar", length(tytul))
            }
            else if(stri_detect_fixed(linki[i], "Glob")){
                  nag <- rep("zloty glob", length(tytul))
            }

            wynik <- rbind(wynik, data.frame(tytul = tytul, nagroda = nag, stringsAsFactors = FALSE))
      }

      #wyrzucam smieci dodane do tytulow
      wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(video)", "")
      wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(miniserial)", "")
      wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(spektakl)", "")
      wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(dokument muzyczny)", "")
      wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(serial dokumentalny)", "")
      wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(animacja)", "")

      wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\(Teatr Telewizji [0-9]+\\)", "")
      wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\(Teatr Telewizji\\)", "")
      wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\([0-9]+\\)", "")
      wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\(.*?film.*?\\)", "")
      wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\(film.*?\\)", "")

      # zamieniam cudzyslowy " na '
      wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\"", "'")

      # usuwam biale znaki z przodu i tylu tytulu
      wynik$tytul <- stri_trim_both(wynik$tytul)

      # usuwam duplikaty
      wynik <- wynik[!duplicated(wynik[,c('tytul')]),]

      return(wynik)
}
