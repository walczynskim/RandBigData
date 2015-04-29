#' Znajdz obsade filmow na Wikipedii.
#'
#' Funkcja \code{wikipedia_charakterystyki_obsada} szuka informacji o obsadzie filmow na Wikipedii.
#'
#' @param filmy_wiki Ramka danych o kolumnach z nazwami "tytul_wikipedia", "link_wikipedia", "rok_produkcji". Taka ramke danych mozna otrzymac korzystajac z funkcji \code{wikipedia_tytul_rok_link}.
#' @param sciezka_wyjscie Sciezka do pliku tekstowego, w ktorym chcemy zapisac pobrane dane.
#' @return Funkcja \code{wikipedia_charakterystyki_obsada} zwraca ramke danych o 3 kolumnach zawierajaca informacje o obsadzie danych filmow znajdujacych sie na Wiki.
#' @details Kolumna "link_wikipedia" argumentu \code{filmy_wiki} powinna zawierac linki do filmow z Wikipedii.

wikipedia_charakterystyki_obsada <- function(filmy_wiki, sciezka_wyjscie){
  obsada <- data.frame(aktor = character(0), postac = character(0), tytul_wikipedia = character(0), stringsAsFactors = FALSE)
  if(nrow(filmy_wiki) == 0){return(obsada)}

  strona_film <- html("http://pl.wikipedia.org/wiki/Mi%C4%99dzy_%C5%9Bwiatami_(film_2010)")
  oni <- html_nodes(strona_film, "ul:nth-child(10) li")
  oni <- html_text(oni)
  a <- c("—", "-", "-", "-", "jako", "–")

  for(i in 1:nrow(filmy_wiki)){
    strona_film <- html(filmy_wiki$link_wikipedia[i])
    oni <- html_nodes(strona_film, "ul:nth-child(10) li")
    oni <- html_text(oni)
    if(!is.na(oni[1]) | length(oni) == 0){
      if(sum(!stri_detect_regex(oni, stri_paste(a, collapse = "|"))) >= 0.5*length(oni)){
        oni <- html_nodes(strona_film, "ul:nth-child(8) li")
        oni <- html_text(oni)
      }
    }
    if(!is.na(oni[1]) | length(oni) == 0){
      if(sum(!stri_detect_regex(oni, stri_paste(a, collapse = "|"))) >= 0.5*length(oni)){
        oni <- html_nodes(strona_film, "ul:nth-child(12) li")
        oni <- html_text(oni)
      }
    }
    if(!is.na(oni[1]) | length(oni) == 0){
      if(sum(!stri_detect_regex(oni, stri_paste(a, collapse = "|"))) >= 0.5*length(oni)){
        oni <- html_nodes(strona_film, "ul:nth-child(7) li")
        oni <- html_text(oni)
      }
    }
    if(!is.na(oni[1]) | length(oni) == 0){
      if(sum(!stri_detect_regex(oni, stri_paste(a, collapse = "|"))) >= 0.5*length(oni)){
        oni <- html_nodes(strona_film, "ul:nth-child(4) li")
        oni <- html_text(oni)
      }
    }
    if(!is.na(oni[1]) | length(oni) == 0){
      if(sum(!stri_detect_regex(oni, stri_paste(a, collapse = "|"))) >= 0.5*length(oni)){
        oni <- html_nodes(strona_film, "ul:nth-child(5) li")
        oni <- html_text(oni)
      }
    }
    if(is.na(oni[1]) | length(oni) == 0 | sum(!stri_detect_regex(oni, stri_paste(a, collapse = "|"))) >= 0.5*length(oni)){
      next
    }
    podziel <- stri_split_regex(oni, stri_paste(a, collapse = "|"))
    podziel <- lapply(podziel, stri_trim_both)

    obsada <- rbind(obsada, data.frame(aktor = sapply(podziel, function(x){x[1]}),
                                       postac = sapply(podziel, function(x){x[2]}),
                                       tytul_wikipedia = rep(filmy_wiki$tytul_wikipedia[i], length(podziel))))
  }
  write.table(obsada, sciezka_wyjscie, quote = TRUE, sep = ",",
              row.names = FALSE, col.names = TRUE)
}
