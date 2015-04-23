#' Pobierz liste filmow z portalu Wikipedia.
#'
#' Funkcja \code{wikipedia_tytul_rok_link} szuka filmow z danego roku (lat) na Wikipedii.
#'
#' @param lata Wektor liczb calkowitych. Funkcja pobierze filmy z Wikipedii ze wszystkich lat podanych w wektorze \code{lata}.
#' @param sciezka_wyjscie Sciezka do pliku tekstowego, w ktorym chcemy zapisac pobrane dane.
#' @return Funkcja \code{wikipedia_tytul_rok_link} zwraca ramke danych o trzech kolumnach. Pierwsza kolumna to wektor napisow o nazwie \code{tytul_wikipedia} zawierajaca tytyly filmow znalezionych przez funkcje, druga kolumna to wektor numeryczny \code{rok_produkcji} zawierajaca rok produkcji pobranych filmow, zas trzecia kolumna to wektor napisow o nazwie \code{link_wikipedia}, ktora przechowuje linki do strony danego filmu na Wikipedii.
#' @details Filmy na Wikipedii sa skatalogowane od 1887 roku. Dlatego sensownie jest za argument lata podac wektor z liczbami calkowitymi wiekszymi badz rownymi 1887.


wikipedia_tytul_rok_link <- function(lata, sciezka_wyjscie){

  akt_rok <- format(Sys.time(), "%Y")

  filmy <- data.frame(tytul_wikipedia = character(0), rok_produkcji = character(0), link_wikipedia = character(0), stringsAsFactors = FALSE)

  if(all(lata < 1887 | lata > akt_rok)){
    return(filmy)
  }

  # wczytuje strone Wiki z kategoriami fimow wg roku

  strona <- html("http://pl.wikipedia.org/wiki/Kategoria:Filmy_wed%C5%82ug_roku_premiery")
  linki_1 <- html_nodes(strona, "li+ li .CategoryTreeLabelCategory")
  linki_2 <- html_attr(linki_1, "href")

  # linki_3 to adresy stron z filmami pochodzącymi z danego roku (129 lat filmografii)

  linki_3 <- stri_paste("http://pl.wikipedia.org", linki_2)
  roki <- stri_extract_first_regex(linki_2, "[0-9]+")
  w <- roki %in% lata

  # ograniczam sie do filmow podanych w wektorze lata

  linki_3 <- linki_3[w]
  roki <- roki[w]

  for(i in 1:length(linki_3)){
    strona_kategorie <- html(linki_3[i])
    linki_kategorie_1 <- html_nodes(strona_kategorie, ".CategoryTreeLabelCategory")
    if(length(linki_kategorie_1) > 0){
      linki_kategorie_2 <- html_attr(linki_kategorie_1, "href")
      linki_kategorie_3 <- stri_paste("http://pl.wikipedia.org", linki_kategorie_2)
      ile_stron_1 <- html_nodes(strona_kategorie, ".CategoryTreeLabelCategory+ span")
      ile_stron_2 <- html_text(ile_stron_1)
      ile_stron_3 <- as.numeric(stri_extract_first_regex(ile_stron_2, "[0-9]+"))
      ile_stron <- ceiling(ile_stron_3/200)
      for(j in 1:length(linki_kategorie_3)){
        n <- ile_stron[j]
        while(n >= 1){
          if(ile_stron[j] == 1){
            strona_kraje <- html(linki_kategorie_3[j])
            linki_filmy_1 <- html_nodes(strona_kraje, "#mw-pages a")
            linki_filmy_2 <- html_attr(linki_filmy_1, "href")
            linki_filmy_3 <- stri_paste("http://pl.wikipedia.org", linki_filmy_2)
            tytuly_filmy <- html_text(linki_filmy_1)

            filmy <- rbind(filmy, data.frame(tytul_wikipedia = tytuly_filmy,
                                             rok_produkcji = rep(roki[i], length(tytuly_filmy)),
                                             link_wikipedia = linki_filmy_3,
                                             stringsAsFactors = FALSE))
            n <- n-1
          }
          else if(n != 1 & ile_stron[j] != 1){
            strona_kraje <- html(linki_kategorie_3[j])
            linki_filmy_1 <- html_nodes(strona_kraje, "#mw-pages a")
            linki_filmy_2 <- html_attr(linki_filmy_1, "href")
            linki_filmy_3 <- stri_paste("http://pl.wikipedia.org", linki_filmy_2)
            tytuly_filmy <- html_text(linki_filmy_1)
            nn <- length(tytuly_filmy)

            filmy <- rbind(filmy, data.frame(tytul_wikipedia = tytuly_filmy[-c(1,nn)],
                                             rok_produkcji = rep(roki[i], length(tytuly_filmy)-2),
                                             link_wikipedia = linki_filmy_3[-c(1,nn)],
                                             stringsAsFactors = FALSE))
            n <- n-1

            linki_kategorie_3[j] <- linki_filmy_3[1]
          }
          else if(n == 1 & ile_stron[j] != 1){
            strona_kraje <- html(linki_kategorie_3[j])
            linki_filmy_1 <- html_nodes(strona_kraje, "#mw-pages a")
            linki_filmy_2 <- html_attr(linki_filmy_1, "href")
            linki_filmy_3 <- stri_paste("http://pl.wikipedia.org", linki_filmy_2)
            tytuly_filmy <- html_text(linki_filmy_1)
            nn <- length(tytuly_filmy)

            filmy <- rbind(filmy, data.frame(tytul_wikipedia = tytuly_filmy[-c(1,nn)],
                                             rok_produkcji = rep(roki[i], length(tytuly_filmy)-2),
                                             link_wikipedia = linki_filmy_3[-c(1,nn)],
                                             stringsAsFactors = FALSE))
            n <- n-1
          }
        }
      }
    }
  }

  #wyrzucam smieci dodane do tytulow

  filmy$tytul_wikipedia <- stri_replace_all_fixed(filmy$tytul_wikipedia, "(video)", "")
  filmy$tytul_wikipedia <- stri_replace_all_fixed(filmy$tytul_wikipedia, "(miniserial)", "")
  filmy$tytul_wikipedia <- stri_replace_all_fixed(filmy$tytul_wikipedia, "(spektakl)", "")
  filmy$tytul_wikipedia <- stri_replace_all_fixed(filmy$tytul_wikipedia, "(dokument muzyczny)", "")
  filmy$tytul_wikipedia <- stri_replace_all_fixed(filmy$tytul_wikipedia, "(serial dokumentalny)", "")
  filmy$tytul_wikipedia <- stri_replace_all_fixed(filmy$tytul_wikipedia, "(animacja)", "")

  filmy$tytul_wikipedia <- stri_replace_all_regex(filmy$tytul_wikipedia, "\\(Teatr Telewizji [0-9]+\\)", "")
  filmy$tytul_wikipedia <- stri_replace_all_regex(filmy$tytul_wikipedia, "\\(Teatr Telewizji\\)", "")
  filmy$tytul_wikipedia <- stri_replace_all_regex(filmy$tytul_wikipedia, "\\([0-9]+\\)", "")
  filmy$tytul_wikipedia <- stri_replace_all_regex(filmy$tytul_wikipedia, "\\(.*?film.*?\\)", "")
  filmy$tytul_wikipedia <- stri_replace_all_regex(filmy$tytul_wikipedia, "\\(film.*?\\)", "")

  # zamieniam cudzyslowy " na '

  filmy$tytul_wikipedia <- stri_replace_all_regex(filmy$tytul_wikipedia, "\"", "'")

  # usuwam biale znaki z przodu i tylu tytulu

  filmy$tytul_wikipedia <- stri_trim_both(filmy$tytul_wikipedia)

  # usuwam duplikaty

  filmy <- filmy[!duplicated(filmy[,c('link_wikipedia')]),]

  write.table(filmy, sciezka_wyjscie, quote = TRUE, sep = ",",
              row.names = FALSE, col.names = TRUE)
}
