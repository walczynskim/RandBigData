#' Znajdowanie napisow do dowolnego filmu poprzez strone www.opensubtitles.org
#'
#' Funkcja \code{znajdz} znajduje sciezke do strony gdzie sa napisy do szukanego filmu.
#'
#' @param nazwa_filmu Wektor napisow z nazwa szukanego flmu.
#' @return sciezka do strony interenetowej gdzie znajduja sie napisy szukanego flimu
#' 
#' @author Adrianna Sudol
#' 
#' @examples
#' znajdz('titanic')
#' znajdz('Ala ma kota')
#' 
#' @import stringi


znajdz = function(nazwa_filmu){
  
  if (missing(nazwa_filmu)){
    cat('Nie podano argumentu')
  }
  
  url = 'http://www.opensubtitles.org/pl/search2/sublanguageid-pol/moviename-'
  nazwa_filmu = stri_trim_both(nazwa_filmu)
  nazwa_filmu = stri_split(nazwa_filmu,regex=" ")
  nazwa_filmu = unlist(nazwa_filmu)
  if (length(nazwa_filmu)==1){
    sciezka = stri_paste(url,nazwa_filmu)
    return(sciezka)
  }else{
    pom = stri_paste(nazwa_filmu,collapse='+')
    sciezka = stri_paste(url,pom)
    return(sciezka)
  }
  
}



