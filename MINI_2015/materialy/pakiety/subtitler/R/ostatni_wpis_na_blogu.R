#' Pobierz ostatni wpis na blogu .subtitler.org
#'
#' Funkcja \code{ostatni_wpis_na_blogu} pobiera ostatni wpis na blogu i komentarze do niego.
#'
#' @return wektor napisow
#' @author Marta Sommer
#' @import dplyr
#' @import rvest

ostatni_wpis_na_blogu <- function(){

  strona <- "http://blog.opensubtitles.org/"

  strona %>%
    html() %>%
    html_nodes("#recent-posts-2 li:nth-child(1) a") %>%
    html_attr("href") -> link

  link %>%
    html() %>%
    html_nodes("p") %>%
    html_text()

}



