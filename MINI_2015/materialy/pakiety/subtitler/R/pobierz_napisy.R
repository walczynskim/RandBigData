#' Pobieranie napisow
#'
#' Funkcja \code{pobierz_napisy} pobiera do folderu napisy do filmow pod wskazanym jezykiem.
#'
#' @param tytul zmienna typu character, case-insensitive, wskazujaca napisy jakiego filmu nas interesuja.
#' Tytuly filmow nie musza byc w oryginalnym jezyku.
#' @param jezyk szukanych napisow.
#' @details Pobieranie napisow z portalu opensubtitles nie jest trywialna sprawa. Link przekierowujacy
#' uzyskany funkcja \code{znajdz_tytul_jezyk}() nie zawsze przekierowuje od razu pod adres z ktorego mozna
#' pobrac zipa z napisami: czesto nalezy kliknac jeszcze raz w link z filmem. Funkcja obsluguje ten przypadek.
#' Ponadto uzytkownik nie jest zmuszony podawac pelna nazwe filmu, jesli jej nie pamieta, np. wystarczy ze poda
#' w tytule "Shrek" a program sam dopyta czy chodzilo o "Shrek 2", "Shrek Forever" czy starego dobrego
#' Shreka, ktory dopiero uwalnia ksiezniczke z wiezy.
#' @return folder z zawierajacy napisy do filmow z oryginalna nazwa pobrana z opensubtitles.
#' @author Katarzyna Fak
#'
#' @examples
#' pobierz_napisy("La vita e bella","pol")
#' pobierz_napisy("Madagaskar","jpn")  # napisy w jezyku japonskim.
#' @import rvest
#' @import stringi
#' @export

pobierz_napisy <- function(tytul, jezyk){
   if (missing(tytul) | missing(jezyk))
      stop('Nie podales wszystkich argumentow')
   url <- znajdz_tytul_jezyk(tytul, jezyk)
   # wczytanie wyszukanych na stronie url linkow i tytulow filmow. Funkcje wykonamy, jesli jakiekolwiek
   # sie znalazly.
   hrefs_titles <- linki_i_tytuly_na_stronie(url)
   if( length(hrefs_titles)>0 ){
      ktory_tytul <- hrefs_titles["title",]
      # Co jesli uzytkownik podal film "Shrek" a program nie wie, czy chodzilo mu o
      # Shrek - Shrek, czy "Shrek Forever" czy "Shrek 2"?
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
   return(pobierz_attrs2["data-installer-file-name"])
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
   download.file(paste0(pobierz_attrs["rel"], ".zip"),tmp)
   if( dir_name%in%dir() )
      message("Uwaga, nadpisanie istniejacych plikow!") else
         dir.create(dir_name)
   unzip(tmp, exdir=dir_name)
   unlink(tmp)
   message("Napisy pobrano, szefie.")
}

#przydaje sie w sentyment_dla_filmu

pobierz_napisy_pierwsze_mozliwe <- function(tytul, jezyk){
   if (missing(tytul) | missing(jezyk))
      stop('Nie podales wszystkich argumentow')
   url <- znajdz_tytul_jezyk(tytul, jezyk)
   # wczytanie wyszukanych na stronie url linkow i tytulow filmow. Funkcje wykonamy, jesli jakiekolwiek
   # sie znalazly.
   hrefs_titles <- linki_i_tytuly_na_stronie(url)
   if( length(hrefs_titles)>0 ){
      ktory_tytul <- hrefs_titles["title",]
      #             # Co jesli uzytkownik podal film "Shrek" a program nie wie, czy chodzilo mu o
      #             # Shrek - Shrek, czy "Shrek Forever" czy "Shrek 2"?
      #             if( !all(duplicated(ktory_tytul)[-1]) ){
      #                   print(ktory_tytul)
      #                   cat("Ktorych napisow potrzebujesz?")
      #                   ktory_film <- as.numeric(scan(file = "", what = "", nmax = 1))
      #             }else
      ktory_film <- 1
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
   return(pobierz_attrs2["data-installer-file-name"])
}