#' Pobierz liste filmow z portalu Filmweb
#'
#' Funkcja \code{filmweb_tytul_rok_link()} zapisuje na dysku najczesciej komentowane filmy na Filmwebie.
#'
#' @param poczatek numer filmu, od ktorego chcemy zaczac pobieranie (domyslnie 1)
#' @param koniec numer filmu, na ktorym chcemy skonczyc pobieranie (domyslnie wszystkie dostepne)
#' @param sciezka_do_pliku_txt_gdzie_zapisac sciezka do pliku .txt, w ktorym chcemy zapisac nasza ramke danych (zapisywanie odbywa sie na biezaco - tak wiec w przypadku wystapienia bledu, wszystkie sciagniete do tej pory dane zostana zapisane)
#' @param czy_dopisac_do_pliku jesli \code{TRUE} dane zostana dopisane, a nie nadpisane do istniejacego pliku (zostana wtedy zapisane bez nazw kolumn)
#' @return Funkcja \code{filmweb_tytul_rok_link} zwraca ramke danych o trzech kolumnach. Pierwsza kolumna to \code{tytul_filmweb} zawierajaca tytyly filmow znalezionych przez funkcje, druga to \code{rok_produkcji} z rokiem produkcji filmu, a trzecia o nazwie \code{link_filmweb} przechowuje linki do strony danego filmu na Filmwebie.

filmweb_tytul_rok_link <- function(poczatek = 1, koniec = NULL,
                                   sciezka_do_pliku_txt_gdzie_zapisac,
                                   czy_dopisac_do_pliku = FALSE){

  adres_strony <- "http://www.filmweb.pl/search/film"
  html_strony <- html(adres_strony)

  ile_stron_wynikow <- html_text(html_nodes(html_strony, "#resultsCount"))
  ile_filmow_na_stronie <- as.numeric(stri_match_all_regex(ile_stron_wynikow, "- (.*) z")[[1]][2])
  pierwsza_iteracja <- ceiling(poczatek / ile_filmow_na_stronie)
  if(is.null(koniec)) koniec <- as.numeric(stri_match_all_regex(ile_stron_wynikow, "z (.*)$")[[1]][2])
  ile_iteracji <- ceiling(koniec / ile_filmow_na_stronie)

  adres_strony_baza <- "http://www.filmweb.pl/search/film?q=&type=&startYear=&endYear=&countryIds=&genreIds=&startRate=&endRate=&startCount=&endCount=&sort=COUNT&sortAscending=false&c=portal&page="

  for(i in pierwsza_iteracja:ile_iteracji){

    cat(i, "/", ile_iteracji, "\n")

    html_itej_strony <-  html(stri_paste(adres_strony_baza, i))
    dane_itej_strony <- html_nodes(html_itej_strony, ".gwt-filmPage")

    tytul <- html_text(dane_itej_strony)
    tytul <- stri_replace_all_fixed(tytul, "\"", "'")

    link <- stri_paste("http://www.filmweb.pl", html_attr(dane_itej_strony, "href"))

    rok <- html_text(html_nodes(html_itej_strony, ".titleYear"))
    rok <- unlist(stri_extract_all_regex(rok, "[0-9]{4}"))

    if(i == pierwsza_iteracja & i==ile_iteracji){

      dokad <- ifelse(koniec %% ile_filmow_na_stronie == 0, ile_filmow_na_stronie, koniec %% ile_filmow_na_stronie)

      tytul <- tytul[(poczatek %% ile_filmow_na_stronie):dokad]
      rok <- rok[(poczatek %% ile_filmow_na_stronie):dokad]
      link <- link[(poczatek %% ile_filmow_na_stronie):dokad]

    } else{

      if(i == pierwsza_iteracja){

        tytul <- tytul[(poczatek %% ile_filmow_na_stronie):length(tytul)]
        rok <- rok[(poczatek %% ile_filmow_na_stronie):length(rok)]
        link <- link[(poczatek %% ile_filmow_na_stronie):length(link)]

      }

      if(i == ile_iteracji){

        tytul <- tytul[1:(koniec %% ile_filmow_na_stronie)]
        rok <- rok[1:(koniec %% ile_filmow_na_stronie)]
        link <- link[1:(koniec %% ile_filmow_na_stronie)]

      }
    }

    ramka <- data.frame(tytul_filmweb = tytul, rok_produkcji = rok, link_filmweb = link)

    if(czy_dopisac_do_pliku == FALSE & i == pierwsza_iteracja){
      write.table(ramka, sciezka_do_pliku_txt_gdzie_zapisac, quote = TRUE, sep = ",",
                  row.names = FALSE, col.names = TRUE)
    } else{
      write.table(ramka, sciezka_do_pliku_txt_gdzie_zapisac, quote = TRUE, sep = ",",
                  row.names = FALSE, col.names = FALSE, append = TRUE)
    }

  }

}
