#' Funkcja wyznacza odpowiednie podobienstwo miedzy dwoma dowolnymi tekstami
#'
#' Funkcja \code{frakcja_powtarzanych_slow} wyznacza odpowiednie podobienstwo
#' miedzy dwoma dowolnymi tekstami
#'
#' @usage frakcja_powtarzanych_slow(text_gl, text_por)
#' @param text_gl wartosc tekstowa, napis
#' @param text_por wartosc tekstowa, napis
#'
#' @return
#' wartosc numeryczna z przedzialu [0,1] - podobienstwo miedzy dwoma tekstami
#'
#'
#'@author Emilia Momotko
#'
#'@import stringi
#'

frakcja_powtarzanych_slow <- function(text_gl, text_por){

  text_gl <- as.character(text_gl)
  text_por <- as.character(text_por)

  if(text_gl=="na"||text_por=="na"||text_gl=="NA"||text_por=="NA"||is.na(text_gl)||is.na(text_por)){

    return(NA)
  }

  text_gl <- unique(unlist(stri_extract_all_words(text_gl)))
  text_por <- unique(unlist(stri_extract_all_words(text_por)))

  pomocniczy <- c(text_gl, text_por)
  2*(sum(stri_duplicated(pomocniczy)))/length(pomocniczy)

}

