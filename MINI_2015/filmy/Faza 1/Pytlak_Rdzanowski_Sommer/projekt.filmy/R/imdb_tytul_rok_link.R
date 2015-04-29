#' Pobierz liste filmow z portalu IMDb
#'
#' Funkcja \code{imdb_tytul_rok_link} zapisuje w pliku tekstowym tytuly najczesciej komentowanych filmow na portalu IMDb, wraz z latami produkcji i linkami do stron poszczegolnych filmow.
#'
#' @param liczba_filmow Liczba tytulow, ktore chcemy pobrac (w kolejnosci malejacej ze wzgledu na liczbe glosow).
#' @param sciezka_wyjscie Sciezka do pliku tekstowego, w ktorym chcemy zapisac pobrane dane.
#' @return Funkcja \code{imdb_tytul_rok_link} zwraca ramke danych o trzech kolumnach. Pierwsza kolumna \code{tytul_imdb} zawiera tytuly pobranych filmow. Druga kolumna \code{rok_produkcji} zawiera informacje o latach powstania filmow. Trzecia kolumna \code{link_imdb} zawiera linki do stron filmow na portalu IMDb.

imdb_tytul_rok_link <- function(liczba_filmow = NULL, sciezka_wyjscie){

   adres1 <- "http://www.imdb.com/search/title?count=250&sort=num_votes,desc&start=1&title_type=feature,tv_movie,documentary,short&view=simple"
   html_adres1 <- html(adres1)

   zakres <- html_nodes(html_adres1, "#left")
   zakres1 <- html_text(zakres)[1]

   filmy_na_stronie <- as.numeric(unlist(stri_extract_all_regex(zakres1, "(?<=\n1-)[0-9]+(?= )")))

   stala_czesc_1 <- "http://www.imdb.com/search/title?count=250&sort=num_votes,desc&start="
   stala_czesc_2 <- "&title_type=feature,tv_movie,documentary,short&view=simple"

   wszystkie_filmy <- unlist(stri_extract_all_regex(zakres1, "[0-9]{3},[0-9]{3}"))
   wszystkie_filmy <- as.numeric(stri_replace_all_regex(wszystkie_filmy, ",", ""))

   if(liczba_filmow < wszystkie_filmy){

      liczba_stron <- ceiling(liczba_filmow/filmy_na_stronie)

      if(liczba_filmow > filmy_na_stronie){

         for(i in 1:(liczba_stron - 1)){

            adres <- stri_paste(stala_czesc_1, filmy_na_stronie*(i - 1) + 1, stala_czesc_2)
            html_adres <- html(adres)

            tytuly_wezly <- html_nodes(html_adres, ".title a")
            tytuly <- html_text(tytuly_wezly)

            lata_wezly <- html_nodes(html_adres, ".year_type")
            lata <- unlist(stri_extract_all_regex(html_text(lata_wezly), "[0-9]{4}"))

            linki_wezly <- tytuly_wezly %>% html_attr("href")
            linki <- stri_paste("http://www.imdb.com", linki_wezly)

            ramka <- data.frame(tytul_imdb = tytuly, rok_produkcji = lata, link_imdb = linki)

            if(i == 1){
               write.table(ramka, sciezka_wyjscie, quote = TRUE, sep = ",",
                           row.names = FALSE, col.names = TRUE)
            } else {
               write.table(ramka, sciezka_wyjscie, append = TRUE, quote = TRUE, sep = ",",
                           row.names = FALSE, col.names = FALSE)
            }
         }

         adres_ost <- stri_paste(stala_czesc_1, filmy_na_stronie*(liczba_stron - 1) + 1,
                                 stala_czesc_2)
         html_adres_ost <- html(adres_ost)

         reszta <- liczba_filmow - (liczba_stron - 1)*filmy_na_stronie

         tytuly_ost_wezly <- html_nodes(html_adres_ost, ".title a")
         tytuly_ost <- html_text(tytuly_ost_wezly)
         tytuly_reszta <- tytuly_ost[1:reszta]

         lata_ost_wezly <- html_nodes(html_adres_ost, ".year_type")
         lata_ost <- unlist(stri_extract_all_regex(html_text(lata_ost_wezly), "[0-9]{4}"))
         lata_reszta <- lata_ost[1:reszta]

         linki_ost_wezly <- tytuly_ost_wezly %>% html_attr("href")
         linki_ost <- stri_paste("http://www.imdb.com", linki_ost_wezly)
         linki_reszta <- linki_ost[1:reszta]

         ramka <- data.frame(tytul_imdb = tytuly_reszta, rok_produkcji = lata_reszta,
                             link_imdb = linki_reszta)

         write.table(ramka, sciezka_wyjscie, quote = TRUE, sep = ",",
                     row.names = FALSE, col.names = FALSE)

      } else {

         tytuly_wezly <- html_nodes(html_adres1, ".title a")
         tytuly <- html_text(tytuly_wezly)
         tytuly_pierw <- tytuly[1:liczba_filmow]

         lata_wezly <- html_nodes(html_adres1, ".year_type")
         lata <- unlist(stri_extract_all_regex(html_text(lata_wezly), "[0-9]{4}"))
         lata_pierw <- lata[1:liczba_filmow]

         linki_wezly <- tytuly_wezly %>% html_attr("href")
         linki <- stri_paste("http://www.imdb.com", linki_wezly)
         linki_pierw <- linki[1:liczba_filmow]

         ramka <- data.frame(tytul_imdb = tytuly_pierw, rok_produkcji = lata_pierw, link_imdb =
                                linki_pierw)

         write.table(ramka, sciezka_wyjscie, quote = TRUE, sep = ",",
                     row.names = FALSE, col.names = TRUE)
      }

   } else {

      liczba_stron <- ceiling(wszystkie_filmy/filmy_na_stronie)

      for(i in 1:liczba_stron){

         adres <- stri_paste(stala_czesc_1, filmy_na_stronie*(i - 1) + 1, stala_czesc_2)
         html_adres <- html(adres)

         tytuly_wezly <- html_nodes(html_adres, ".title a")
         tytuly <- html_text(linki_wezly)

         lata_wezly <- html_nodes(html_adres, ".year_type")
         lata <- unlist(stri_extract_all_regex(html_text(lata_wezly), "[0-9]{4}"))

         linki_wezly <- tytuly_wezly %>% html_attr("href")
         linki <- stri_paste("http://www.imdb.com", linki_wezly)

         ramka <- data.frame(tytul_imdb = tytuly, rok_produkcji = lata, link_imdb = linki)

         if(i == 1){
            write.table(ramka, sciezka_wyjscie, quote = TRUE, sep = ",",
                        row.names = FALSE, col.names = TRUE)
         } else {
            write.table(ramka, sciezka_wyjscie, append = TRUE, quote = TRUE, sep = ",",
                        row.names = FALSE, col.names = FALSE)
         }
      }
   }

}
