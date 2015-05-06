#' Funkcja \code{wyszynska_rudas} buduje chume slow z napisow do filmu podanego jako tytul
#'
#' @usage wyszynska_rudas(tytul, jezyk)
#'
#' @param tytul tytul filmu
#' @param jezyk jezyk, w  ktorym maja zostac pobrane napisy (patrz wymagania funkcji pobierz_napisy)
#'
#' @author Karolina Wyszynska & Krzysztof Rudas
#' @examples
#' wyszynska_rudas("Lord of the Rings", "eng")
#' wyszynska_rudas("lego przygoda", "pol")
#' wyszynska_rudas("The Ring", "eng")
#' wyszynska_rudas("Kraina Lodu", "eng")
#' wyszynska_rudas("Shrek", "pol")
#'
#' @import tm
#' @import wordcloud
#' @import rvest
#' @import stringi
#'

wyszynska_rudas <- function(tytul, jezyk){

  pobierz_napisy_2(tytul,jezyk )

  #Dostajemy sie do pliku z napisami
  pliki <- list.files(getwd(), pattern="srt$|txt$", recursive=TRUE, full.names=TRUE)
  daty <- numeric(length(pliki))
  for(i in seq_along(daty)){
    daty[i] <- as.numeric(file.info(pliki[i])$ctime)
  }

  plik <- pliki[which(daty==max(daty))]
  #Oczyszczamy napisy
  tekst <- stri_paste(readLines(plik), collapse=" ")
  tekst <- stri_replace_all_regex(tekst, "[0-9]|:|-|>|,", " ")

  #Budujemy chmure
  wordcloud(tekst, scale=c(4,0.05), max.words=50,
            random.order=FALSE, rot.per=0.35, use.r.layout=FALSE,
            colors=brewer.pal(8, "Dark2"))
}


pobierz_napisy_2 <- function(tytul, jezyk){
  if (missing(tytul) | missing(jezyk))
    stop('Nie podales wszystkich argumentow')
  url <- znajdz_tytul_jezyk2(tytul, jezyk)
  # wczytanie wyszukanych na stronie url linkow i tytulow filmow. Funkcje wykonamy, jesli jakiekolwiek
  # sie znalazly.
  hrefs_titles <- linki_i_tytuly_na_stronie2(url)
  if( length(hrefs_titles)>0 ){
    ktory_tytul <- hrefs_titles["title",]
    ktory_film <- 1
    url_napisow <- paste0("http://opensubtitles.org", hrefs_titles["href", ktory_film])
    pobierz_attrs <- atrybuty_pobierz2(url_napisow)
    if( !is.null(pobierz_attrs) ){
      pobierz_rozpakuj2(pobierz_attrs)
      # Normalnie napisy powinny pobrac sie juz w tym kroku, jednak czesto na stronie
      # opensubtitles.org trzeba kliknac w jeszcze jeden link np. dla filmow
      # "La vita e bella" lub "Titanic". Jezeli atrybuty pobierania w tym kroku
      # ustawione sa na NULL (pomimo, ze stri_detektor wykryl film) nalezy wykonac
      # jeszcze jedno przejscie wglab strony.
    }else{
      hrefs_titles_zagniezdzone <- linki_i_tytuly_na_stronie2(url_napisow)
      if(length(hrefs_titles_zagniezdzone)==0)
        stop("Niedokladne zapytanie, mozliwe, ze szukasz serialu?")
      # czy_znaleziono_tytul <- stri_detect_regex(tolower(href_n_title[2,]), tolower(tytul))
      # nie trzeba sprawdzac, czy znaleziono tytul, bo na to juz wskazuje sam fakt ze sie wyszukalo.
      zip_url <- paste0("http://opensubtitles.org",hrefs_titles_zagniezdzone["href", 1]  )
      pobierz_attrs2 <- atrybuty_pobierz2(zip_url)
      if( !is.null(pobierz_attrs2) ){
        pobierz_rozpakuj2(pobierz_attrs2)
      }else stop("Nie moge dogrzebac sie do Twoich napisow... ;/")
    }

  }else{
    stop("Nie ma napisow dla tego filmu lub w tym jezyku ;(")
  }
}

linki_i_tytuly_na_stronie2 <- function(url){
  dostepne_napisy <- html_nodes(html(url),".bnone")
  attrs <- html_attrs(dostepne_napisy)
  hrefs_titles <- sapply(attrs, function(x) x[c("href","title")])
  return(hrefs_titles)
}

atrybuty_pobierz2 <- function(url_napisow){
  pobierz_button <- html_nodes(html(url_napisow),"#bt-dwl")
  attrs2 <- html_attrs(pobierz_button)
  pobierz <- unlist(attrs2)
  return(pobierz)
}

pobierz_rozpakuj2 <- function(pobierz_attrs){
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
znajdz_tytul_jezyk2 <- function(tytul, jezyk){

  if (missing(tytul) | missing(jezyk))
    stop('Nie podano argumentu')
  url <- paste0('http://www.opensubtitles.org/pl/search2/sublanguageid-', jezyk,'/moviename-')
  title <- strsplit(tytul," ")[[1]]
  sciezka <- paste0(url, ifelse(length(title)==1, title, paste0(title,collapse = "+")))
  return(sciezka)
}
