#' Pobierz podstawowe informacje na temat wybranych filmow na portalu IMDb
#'
#' Funkcja \code{imdb_charakterystyki} znajduje podstawowe informacje na temat wybranych filmow znajdujacych sie w bazie portalu IMDb i zapisuje je w pliku tekstowym.
#'
#' @param sciezka_wejscie Plik tekstowy zawierajace nastepujace dane: tytuly filmow, lata produkcji, linki do stron filmow na portalu IMDb.
#' @return Funkcja \code{imdb_charakterystyki} zwraca ramke danych zawierajaca nastepujace dane: tytul filmu, rok produkcji, link, polski tytul, czas trwania (w minutach), ocena (w skali od 1 do 10), liczba glosow, liczba recenzji uzytkownikow, liczba recenzji krytykow, krotkie streszczenie.


imdb_charakterystyki <- function(sciezka_wejscie, sciezka_wyjscie){

   dane <- read.table(sciezka_wejscie, header = TRUE, sep = ",")
   linki <- as.character(dane[, 3])
   n <- nrow(dane)

   tytuly_pl <- character(n)
   czasy_trwania <- numeric(n)
   oceny <- numeric(n)
   liczby_glosow <- numeric(n)
   liczby_recenzji_uzyt <- numeric(n)
   liczby_recenzji_kryt <- numeric(n)
   streszczenia <- character(n)

   for(i in seq_along(linki)){

      html_link <- html(linki[i])

      info <- html_nodes(html_link, ".infobar")
      info <- html_text(info)

      tytul_pl <- html_nodes(html_link, ".header .itemprop")
      tytuly_pl[i] <- html_text(tytul_pl)

      czas_trwania <- stri_extract_first_regex(info, "[0-9]+? min")
      czasy_trwania[i] <- as.numeric(stri_replace_all_regex(czas_trwania, " min", ""))

      ocena <- html_nodes(html_link, "strong span")
      oceny[i] <- as.numeric(html_text(ocena))

      glosy <- html_nodes(html_link, ".star-box-details a:nth-child(3)")
      glosy <- stri_trim_left(stri_replace_all_regex(html_text(glosy), " users\n", ""))
      liczby_glosow[i] <- as.numeric(stri_replace_all_regex(glosy, " ", ""))

      recenzje_uzyt <- html_nodes(html_link, ".star-box-details a:nth-child(6) span")
      recenzje_uzyt <- stri_replace_all_regex(html_text(recenzje_uzyt), " user", "")
      recenzje_uzyt <- as.numeric(stri_replace_all_regex(recenzje_uzyt, " ", ""))

      if(length(recenzje_uzyt) == 0){
         liczby_recenzji_uzyt[i] <- NA
      } else {
         liczby_recenzji_uzyt[i] <- recenzje_uzyt
      }

      recenzje_kryt <- html_nodes(html_link, "#overview-top a:nth-child(8) span")
      recenzje_kryt <- stri_replace_all_regex(html_text(recenzje_kryt)[2], " critic", "")
      recenzje_kryt <- as.numeric(stri_replace_all_regex(recenzje_kryt, " ", ""))

      if(length(recenzje_kryt) == 0){
         liczby_recenzji_kryt[i] <- NA
      } else {
         liczby_recenzji_kryt[i] <- recenzje_kryt
      }

      streszczenie <- html_nodes(html_link, "#overview-top p")
      streszczenia[i] <- stri_replace_all_regex(html_text(streszczenie)[2], "\n", "")

   }

   nowe_kolumny <- data.frame(tytul_pl = tytuly_pl, czas_trwania = czasy_trwania,
                              ocena = oceny, liczba_glosow = liczby_glosow,
                              liczba_recenzji_uzyt = liczby_recenzji_uzyt,
                              liczba_recenzji_kryt = liczby_recenzji_kryt,
                              streszczenie = streszczenia)
   nowa_ramka <- cbind(dane, nowe_kolumny)

   f <- file(sciezka_wyjscie, open = "a")

   write.table(nowa_ramka, file = f, quote = TRUE, sep = ",",
               row.names = FALSE, col.names = TRUE)

   close(f)
}

