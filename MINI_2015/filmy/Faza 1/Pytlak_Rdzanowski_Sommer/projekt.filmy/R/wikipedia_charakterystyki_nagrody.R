#' Znajdz tytuly filmow nagrodzone nagrodami na Wikipedii.
#'
#' Funkcja \code{wikipedia_charakterystyki_nagrody} szuka tytulow filmow na Wikipedii, ktore dostaly Oscara i(lub) Zlotego Globa.
#'
#' @aliases wikipedia_charakterystyki_nagrody
#' @param nagroda Wektor napisow zawierajacy slowa "oscar" lub "zloty glob". Domyslnie \code{nagrody = c("oscar", "zloty glob")}. Slowa inne niz podane sa ignorowane.
#' @param sciezka_wyjscie Sciezka do pliku tekstowego, w ktorym chcemy zapisac pobrane dane.
#' @return Funkcja \code{wikipedia_charakterystyki_nagrody} zwraca ramke danych o 2 kolumnach zawierajaca tytul filmu i nagrode jaka ten film uzyskal.
#' @details Dane na temat nagrodzonych filmow pochodza z Wikipedii.

wikipedia_charakterystyki_nagrody <- function(nagroda = c("oscar", "zloty glob"), sciezka_wyjscie){
  nagroda <- unique(nagroda)
  w_oscar <- "oscar" %in% nagroda
  w_zloty <- "zloty glob" %in% nagroda
  wynik <- data.frame(tytul = character(0), nagroda = character(0), stringsAsFactors = FALSE)
  if(!w_oscar & !w_zloty){return(wynik)}
  linki <- c("http://pl.wikipedia.org/wiki/Kategoria:Filmy_nominowane_do_Oscara",
             "http://pl.wikipedia.org/w/index.php?title=Kategoria:Filmy_nominowane_do_Oscara&pagefrom=Wi%C4%99zie%C5%84+kr%C3%B3lewski#mw-pages",
             "http://pl.wikipedia.org/wiki/Kategoria:Filmy_nagrodzone_Z%C5%82otym_Globem_dla_najlepszego_filmu_dramatycznego")
  nodes <- c(".mw-category-group a", ".mw-category-group a", ".mw-category-group+ .mw-category-group li")

  if(w_oscar & !w_zloty){
    linki <- linki[1:2]
    nodes <- nodes[1:2]
  }
  if(!w_oscar & w_zloty){
    linki <- linki[3]
    nodes <- nodes[1]
  }

  for(i in 1:length(linki)){
    strona <- html(linki[i])
    tytuly <- html_nodes(strona, nodes[i])
    tytul <- html_text(tytuly)
    if(stri_detect_fixed(linki[i], "Oscar")){
      nag <- rep("oscar", length(tytul))
    }
    else if(stri_detect_fixed(linki[i], "Glob")){
      nag <- rep("zloty glob", length(tytul))
    }

    wynik <- rbind(wynik, data.frame(tytul = tytul, nagroda = nag, stringsAsFactors = FALSE))
  }

  #wyrzucam smieci dodane do tytulow
  wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(video)", "")
  wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(miniserial)", "")
  wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(spektakl)", "")
  wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(dokument muzyczny)", "")
  wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(serial dokumentalny)", "")
  wynik$tytul <- stri_replace_all_fixed(wynik$tytul, "(animacja)", "")

  wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\(Teatr Telewizji [0-9]+\\)", "")
  wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\(Teatr Telewizji\\)", "")
  wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\([0-9]+\\)", "")
  wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\(.*?film.*?\\)", "")
  wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\\(film.*?\\)", "")

  # zamieniam cudzyslowy " na '
  wynik$tytul <- stri_replace_all_regex(wynik$tytul, "\"", "'")

  # usuwam biale znaki z przodu i tylu tytulu
  wynik$tytul <- stri_trim_both(wynik$tytul)

  # usuwam duplikaty
  wynik <- wynik[!duplicated(wynik[,c('tytul')]),]

  write.table(wynik, sciezka_wyjscie, quote = TRUE, sep = ",",
              row.names = FALSE, col.names = TRUE)
}
