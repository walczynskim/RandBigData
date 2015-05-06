#' Funkcja zapisuje plik z napisami dla danego filmu,
#' nastepnie bada czestotliwosc wystepowania literek w tychze napisach
#'
#' Funkcja \code{momotko_spiewak(tytul, jezyk)} pobiera napisy ze strony http://www.opensubtitles.org/,
#' nastepnie bada czestotliwosc wystepowania literek w tychze napisach
#'
#'
#' @param tytul wektor jednoelementowy typu character - tytul filmu (jednowyrazowy)
#' @param jezyk wektor jednoelementowy typu character - jezyk filmu
#'
#' @examples
#' momotko_spiewak("shrek","eng")
#'
#' @import rvest
#' @import stringi
#' @import XML
#'
#' @author Emilia Momotko, Martyna śpiewak

momotko_spiewak <- function(tytul, jezyk){
  
  ###########################################################################
  # funkcje Kasi Fąk - kosmetycznie zmienione, wydobywamy od razu nazwe pliku,
  # gdzie sa przechowywane napisy
  
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
        #print(ktory_tytul)
        #cat("Ktorych napisow potrzebujesz?")
        ktory_film <- 1
        #ktory_film <- as.numeric(scan(file = "", what = "", nmax = 1))
      }else ktory_film <- 1
      url_napisow <- paste0("http://opensubtitles.org", hrefs_titles["href", ktory_film])
      pobierz_attrs <- atrybuty_pobierz(url_napisow)
      if( !is.null(pobierz_attrs) ){
        res <- pobierz_rozpakuj(pobierz_attrs)
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
        zip_url <- paste0("http://opensubtitles.org",hrefs_titles_zagniezdzone["href", 1] )
        pobierz_attrs2 <- atrybuty_pobierz(zip_url)
        if( !is.null(pobierz_attrs2) ){
          res <- pobierz_rozpakuj(pobierz_attrs2)
        }else stop("Nie moge dogrzebac sie do Twoich napisow... ;/")
      }
    }else{
      stop("Nie ma napisow dla tego filmu lub w tym jezyku ;(")
    }
    return(res)
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
    return(dir_name)
  }
  
  znajdz_tytul_jezyk <- function(tytul, jezyk){
    if (missing(tytul) | missing(jezyk))
      stop('Nie podano argumentu')
    url <- paste0('http://www.opensubtitles.org/pl/search2/sublanguageid-', jezyk,'/moviename-')
    title <- strsplit(tytul," ")[[1]]
    sciezka <- paste0(url, ifelse(length(title)==1, title, paste0(title,collapse = "+")))
    return(sciezka)
  }

  # pobieramy napisy, funkcja pobierz_napisy zwraca rowniez nazwe folderu
  title <- pobierz_napisy(tytul, jezyk)

  # wczytujemy napisy
  tmp_path <- stri_paste(getwd(), "/", title)
  path <- list.files(tmp_path, c(".srt", ".txt"))
  napisy <- readLines(paste(tmp_path, path, sep="/"))

  # zliczamy sume wystapień kazdej literki
  napisy <- stri_trans_tolower(napisy)
  literki <- na.omit(unlist(stri_extract_all_charclass(napisy, '\\p{Ll}', merge = FALSE, )))
  ile <- table(literki)
  # liczymy czestotliwosc

  czestosc <- ile/sum(ile)
  names(czestosc)

  # wydobycie tytulu
  Title <- unlist(stri_extract_all_regex(title, ".+(?=\\-.*\\-[0-9])"))
  Title <- stri_replace_all_regex(Title, "-" , " ")
  Title <- stri_trans_totitle(Title)

  # wykres czestotliwosci - po angielsku bo opencpu ma problem za znakami
  barplot(czestosc, main = paste0("Subtitle letter frequency for ", Title))

  return(czestosc)
}


##################################################
# PONIZEJ ZAKOMENTOWANY KOD - dzialanie opencpu - wydobycie czestotliwosci literek
# library(stringi)
# library(httr)
# library(opencpu)
# 
# #nalezy podac numer serwera, tytul i jezyk
# server <- 7765
# server <- as.character(server)
# title <- "shrek"
# language <- "eng"
# 
# 
# www <- stri_paste("http://localhost:",server,"/ocpu/library/subtitler/R/momotko_spiewak")
# 
# base <- stri_paste("http://localhost:",server,"/ocpu/tmp/")
# 
# tit <- stri_paste("'",title,"'")
# lan <- stri_paste("'",language,"'")
# 
# (info <- httr::POST(www, body = list(tytul=tit,jezyk=lan)))
# 
# name <- stri_extract_first_regex(rawToChar(info$content), "x.+(?=/R)")
# 
# end1 <- "/R/.val/rda"
# stri_paste(base,name,end1)
# 
# load(url(stri_paste(base,name,end1)))
# (w1 <- .val)
# 
# end2 <- "/R/.val/csv"
# (w2 <- readLines(url(stri_paste(base,name,end2))))
# 
# (tmp <- httr::GET(stri_paste(base,name,"/R")))
# (w3 <- cat(rawToChar(tmp$content)))
