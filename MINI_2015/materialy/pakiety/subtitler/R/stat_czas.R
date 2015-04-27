#' Sprawdz czas wyswietlania napisow w filmie.
#'
#' Funkcja \code{stat_czas} zapisuje napisy do filmow oraz zwraca podstawowe statystyki dla dlugosci wyswietlania napisow.
#'
#' @aliases stat_czas
#' @param link Wektor napisow dlugosci 1. Link do pobrania napisow do filmow pochodzacych ze strony opensubtitles.org.
#' @param sciezka Wektor napisow dlugosci 1. Sciezka do miejsca, w ktorym zapisane zostana napisy.
#' @return Wektor numeryczny dlugosci 6 zawierajacy podstawowe statystyki dla czasu wyswietlania napisow w danym filmie.
#' @details Jezeli argument \code{sciezka} nie zostanie podany to napisy do filmu nie zostana zapisane. Funkcja \code{stat_czas} rysuje dodatkowo czas wyswietlania sie napisow w zaleznosci od czasu trwania filmu z wyrysowanym splinem.
#' @examples
#' link <- "http://dl.opensubtitles.org/pl/download/file/1954081967"
#' sciezka <- stri_paste(getwd(), "/film_przykladowy.txt")
#' statystyki <- stat_czas(link = link, sciezka = sciezka)
#' @author Marcin Rdzanowski
#' @import stringi
#' @import ggplot2
#'

stat_czas <- function(link, sciezka){
  if(!(is.character(link))){
    stop("Argument link musi byc napisem.")
  }
  
  if(!(length(link) == 1)){
    stop("Argument link musi miec dlugosc 1.")
  }
  
  if(!missing(sciezka)){
    if(! (length(sciezka) == 1)){
      stop("Argument sciezka musi miec dlugosc 1 lub nie trzeba go podawac.")
    }
    
    if(! is.character(sciezka)){
      stop("Argument sciezka musi byc napisem lub nie trzeba go podawac.")
    }
  }
  
  # wczytuje napisy ze strony opensubtitles
  napisy <- readLines(link)
  # jesli argument sciezka jest podany to zapisuje napisy
  if(!missing(sciezka)){
    writeLines(tekst, sciezka)
  }
  
  # chce wydobyc dlugosc wyswietlania sie napisow
  ktory_czas <- stri_detect_fixed(napisy, "-->")
  napisy_czas <- napisy[ktory_czas]
  czas <- stri_extract_all_regex(napisy_czas, "[0-9:,]+")
  
  dlg_czas <- sapply(czas, function(x){
    czas_1 <- stri_extract_all_regex(x[1], "[0-9]+")[[1]]
    czas_2 <- stri_extract_all_regex(x[2], "[0-9]+")[[1]]
    time <- as.numeric(czas_2) - as.numeric(czas_1)
    time <- 3600*time[1]+60*time[2]+time[3]+time[4]/1000
    return(time)
  })
  
  os_x <- sapply(czas, function(x){
    czas_1 <- stri_extract_all_regex(x[1], "[0-9]+:[0-9]+:[0-9]+")[[1]]
    return(czas_1)
  })
  
  indeks <- 1:length(dlg_czas)
  w <- seq(from = indeks[1],to = rev(indeks)[1], length.out = 10)
  
  # rysuje wykres czasu wyswietlania napisow od momentu filmu
  ggplot(data.frame(indeks = indeks, czas = dlg_czas), aes(x = indeks, y = czas))+
    geom_smooth(stat = "smooth")+
    geom_point(aes(x = indeks, y = czas))+
    xlab("czas filmu(hh:mm:ss)")+ylab("czas(s)")+
    scale_x_continuous(breaks = indeks[w], labels = os_x[w])
  
  # zwracam podstawowe statystyki dlugosci wyswietlania napisow
  return(summary(dlg_czas))
}
