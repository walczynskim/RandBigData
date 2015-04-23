#' Ostatnio dodane napisy w jezyku romunskim.
#'
#'
#' Funkcja \code{OstatnieRomunskieNapisy()} patrzy na stronie ostatnio dodane napisy do romunskich filmow i zapisuje je do pliku. Kazdy tytul filmu jest zapisywany z jego rokiem premiery.
#'
#'
#'
#' @param hd Zmienna logiczna, jesli TRUE to zwraca napisy tylko dla filmow 'w duzej rozdzielczosci', domyslnie FALSE.
#'
#'
#' @author Pawel Grabowski
#'
#'
#' @examples
#' OstatnieRumunskieNapisy('example.txt')
#' OstatnieRumunskieNapisy('example.txt', hd = TRUE)
#'
#'
#'
#' @import stringi
#' @import dplyr
#' @import rvest


OstatnieRomunskieNapisy <- function(hd = FALSE) {
  stopifnot(is.logical(hd))
  if (hd == TRUE) {
    htmlSite <- html("http://www.opensubtitles.org/pl/search/sublanguageid-rum/searchonlymovies-on/hd-on")
  } else {
    htmlSite <- html("http://www.opensubtitles.org/pl/search/sublanguageid-rum/searchonlymovies-on")
  }
  moviesNames <-  html_nodes(htmlSite, ".bnone") %>%
    html_text() %>%
    stri_replace_all_regex(.,"\n \t\t\t", "")
  return(moviesNames)
}

