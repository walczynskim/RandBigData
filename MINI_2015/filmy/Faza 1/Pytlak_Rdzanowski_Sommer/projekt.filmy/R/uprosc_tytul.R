#' Upraszcza tytuly filmow
#'
#' Funkcja \code{uprosc_tytul()} zwraca wektor tytulow pozbawiony spacji, myslnikow, przecinkow, itd., wszystkie litery sa zamienione na male, a znaki z UTF-8 na znaki z alfabetu lacinskiego.
#'
#' @param wektor_tytulow wektor napisow
#' @return wektor napisow

uprosc_tytul <- function(wektor_tytulow){

  wektor_tytulow %>%
    stri_replace_all_fixed("\u0105", "a") %>%
    stri_replace_all_fixed("\u0107", "c") %>%
    stri_replace_all_fixed("\u0119", "e") %>%
    stri_replace_all_fixed("\u0142", "l") %>%
    stri_replace_all_fixed("\u0144", "n") %>%
    stri_replace_all_fixed("\u00F3", "o") %>%
    stri_replace_all_fixed("\u015B", "s") %>%
    stri_replace_all_fixed("\u017A", "z") %>%
    stri_replace_all_fixed("\u017C", "z") %>%
    stri_replace_all_fixed("\u00FC", "u") %>%
    stri_replace_all_fixed("\u00E7", "c") %>%
    stri_trans_tolower() %>%
    stri_extract_all_words() %>%
    lapply(stri_paste, collapse="") %>%
    unlist()

}
