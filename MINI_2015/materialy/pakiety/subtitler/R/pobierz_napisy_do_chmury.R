#' Pobieranie napisow do chmury slow
#'
#' Funkcja \code{pobierz_napisy_do_chmury} pobiera do folderu napisy do filmow pod wskazanym jezykiem.
#'
#' @param tytul zmienna typu character, case-insensitive, wskazujaca napisy jakiego filmu nas interesuja.
#' Tytuly filmow nie musza byc w oryginalnym jezyku.
#' @param jezyk szukanych napisow.
#' @details Pobieranie pierwszych z brzegu napisow z portalu opensubtitles.
#' @return plik tekstowy z napisami
#' @author Katarzyna Fak
#'
#' @examples
#' pobierz_napisy_do_chmury("La vita e bella","pol")
#' pobierz_napisy_do_chmury("Madagaskar","jpn") # napisy w jezyku japonskim.
#' @import rvest
#' @import stringi
#' @export
pobierz_napisy_do_chmury <- function(tytul, jezyk){
if (missing(tytul) | missing(jezyk))
stop('Nie podales wszystkich argumentow')
url <- znajdz_tytul_jezyk(tytul, jezyk)
# wczytanie wyszukanych na stronie url linkow i tytulow filmow. Funkcje wykonamy, jesli jakiekolwiek
# sie znalazly.
hrefs_titles <- linki_i_tytuly_na_stronie(url)
if( length(hrefs_titles)>0 ){
ktory_film <- 1 # pierwszy z brzegu
url_napisow <- paste0("http://opensubtitles.org", hrefs_titles["href", ktory_film])
pobierz_attrs <- atrybuty_pobierz(url_napisow)
if( !is.null(pobierz_attrs) ){
pobierz_rozpakuj(pobierz_attrs,tytul,jezyk)
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
pobierz_rozpakuj(pobierz_attrs2,tytul,jezyk)
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
pobierz_rozpakuj <- function(pobierz_attrs,tyt,jez){
path <- getwd()
dir_name <- paste0(c(tyt,jez,"napisy"),collapse="_")
tmp <- tempfile()
if(Sys.info()["sysname"]=="Windows")
download.file(paste0(pobierz_attrs["rel"],".zip"),tmp) else
download.file(pobierz_attrs["rel"],tmp)
if( dir_name%in%dir() )
message("Uwaga, nadpisanie istniejacych plikow!") else
dir.create(dir_name)
unzip(tmp, exdir=dir_name)
unlink(tmp)
setwd(dir_name)
ktore_txt <- stri_detect_regex(list.files(recursive = TRUE), ".txt|.srt|.ssa")
napisy <- c(sapply(list.files(recursive = TRUE)[which(ktore_txt)], readLines))
setwd(path)
return(napisy)
}
