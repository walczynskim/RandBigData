#' Pobierz charakterysyki filmow z Filmwebu
#'
#' Funkcja \code{filmweb_charakterystyki()} szuka roznych charakterystyk dla filmow z Filmwebu.
#'
#' @param sciezka_do_tabeli_danych sciezka_dostepu do tabeli zawierajacej kolumne o nazwie \code{link_filmweb} oraz \code{tytul_filmweb}
#' @param gdzie_zapisac_tabele sciezka dostepu do folderu, w ktorym ma byc zapisana ostateczna tabela ze wskaznikami
#' @param gdzie_zapisac_obsade sciezka dostepu do folderu, w ktorym maja byc zapisane pliki .txt z obsada poszczegolnych filmow (beda one zapisane w formie: uproszczonytytulfilmu_rokprodukcji_ocena_obsady.txt)
#' @param poczatek pierwszy wiersz tabeli, ktory chcemy analizowac
#' @param koniec ostatni wiersz tabeli, ktory chcemy analizowac (domyslnie wszystkie wiersze)
#' @param czy_dopisac_do_pliku jesli \code{TRUE} dane zostana dopisane, a nie nadpisane do istniejacego pliku (zostana wtedy zapisane bez nazw kolumn)
#' @return Funkcja \code{filmy_z_filmwebu} zwraca ramke danych o 16 kolumnach (2 wejsciowe, jedna bedaca znacznikiem filmwebu i 13 roznych charakterysyk - box_office, dluzszy_opis_filmu, gatunek, ile_osob_chce_zobaczyc, kraj_produkcji, krotki_opis_filmu, ktory_w_world_rankingu, liczba_komentarzy, liczba_oddanych_glosow, minuty_trwania_filmu, ocena_filmu, premiera_polska, premiera_swiat) zapisana w pliku o rozszerzeniu .txt oraz tyle plikow .txt z obsada i jej ocena, ile bylo filmow w wejsciowej ramce danych, majacych link do Filmwebu.

filmweb_charakterystyki <- function(sciezka_do_tabeli_danych,
                                                    gdzie_zapisac_tabele,
                                                    gdzie_zapisac_obsade,
                                                    poczatek=1,
                                                    koniec=nrow(tabela_danych),
                                                    czy_dopisac_do_pliku=FALSE){

  # ocena filmu:

  filmweb_ocena_filmu <- function(html_linku){
    html_linku %>%
      html_nodes(".light span") %>%
      html_text() %>%
      stri_replace_all_fixed(",", ".") %>%
      as.numeric()
  }

  # liczba oddanych glosow:

  filmweb_liczba_oddanych_glosow <- function(html_linku){
    html_linku %>%
      html_nodes(".text-right div") %>%
      html_text() %>%
      "["(1) %>%
      stri_extract_all_regex("[0-9]") %>%
      unlist() %>%
      stri_paste(collapse="") %>%
      as.numeric()
  }

  # ile osob chce zobaczyc film:

  filmweb_ile_osob_chce_zobaczyc <- function(html_linku){
    html_linku %>%
      html_nodes(".text-right div") %>%
      html_text() %>%
      "["(3) %>%
      stri_extract_all_regex("[0-9]") %>%
      unlist() %>%
      stri_paste(collapse="") %>%
      as.numeric()
  }

  # czas trwania filmu:

  filmweb_minuty_trwania_filmu <- function(html_linku){
    html_linku %>%
      html_nodes(".filmMainHeader") %>%
      html_text() %>%
      stri_match_all_regex(",duration:\\\"([0-9].*?)\\\"[}][)];") %>%
      unlist() %>%
      "["(2) %>%
      as.numeric()
  }

  # krotki opis filmu:

  filmweb_krotki_opis_filmu <- function(html_linku){
    html_linku %>%
      html_nodes(".bottom-15 .text") %>%
      html_text() %>%
      "["(1) %>%
      stri_replace_all_fixed("\"", "'") -> tekst

    if(tekst=="Ten film nie ma jeszcze zarysu fabu\u0142y. Mo\u017Cesz go doda\u0107.") return(NA) else return(tekst)
  }

  # rezyseria:

  filmweb_rezyseria <- function(html_linku){
    html_linku %>%
      html_nodes("tr:nth-child(1) .sep-comma a") %>%
      html_text() %>%
      stri_paste(collapse=",")
  }

  # gatunek:

  filmweb_gatunek <- function(html_linku){
    html_linku %>%
      html_nodes(".genresList a") %>%
      html_text() %>%
      stri_paste(collapse=",")
  }

  # kraj produkcji:

  filmweb_kraj_produkcji <- function(html_linku){
    html_linku %>%
      html_nodes("tr:nth-child(5) .sep-comma a") %>%
      html_text() %>%
      stri_paste(collapse=",") -> kraj

    if(length(kraj)!=0) return(kraj)

    html_linku %>%
      html_text() %>%
      stri_match_all_regex("produkcja:(.*?)var") %>%
      unlist() %>%
      "["(2) -> kraj2

    if(length(kraj2)==0) return(NA) else return(kraj2)
  }

  # data premiery (Polska i swiat):

  filmweb_tabela_dat_premiery <- function(html_linku){
    html_linku %>%
      html_nodes(".bottom-15 tr:nth-child(6) a") %>%
      html_text() -> tekst

    if(length(tekst)==0) return(NULL)

    tekst %>%
      stri_match_all_regex("(.*?)[(](.*?)[)]((.*?)[(](.*?)[)])?") %>%
      unlist() %>%
      "["(-c(1,4)) %>%
      matrix(ncol=2, byrow=TRUE) %>%
      as.data.frame() -> ramka

    colnames(ramka) <- c("kiedy", "gdzie")

    ramka$kiedy %>%
      as.character() %>%
      stri_extract_all_words() %>%
      lapply(function(x) if(length(x)<3) NA else x) %>%
      lapply(rev) %>%
      lapply(stri_paste, collapse="-") %>%
      unlist() %>%
      stri_replace_all_fixed("stycznia", "1") %>%
      stri_replace_all_fixed("lutego", "2") %>%
      stri_replace_all_fixed("marca", "3") %>%
      stri_replace_all_fixed("kwietnia", "4") %>%
      stri_replace_all_fixed("maja", "5") %>%
      stri_replace_all_fixed("czerwca", "6") %>%
      stri_replace_all_fixed("lipca", "7") %>%
      stri_replace_all_fixed("sierpnia", "8") %>%
      stri_replace_all_fixed("wrze\u015Bnia", "9") %>%
      stri_replace_all_fixed("pa\u017Adziernika", "10") %>%
      stri_replace_all_fixed("listopada", "11") %>%
      stri_replace_all_fixed("grudnia", "12") %>%
      as.Date() -> ramka$kiedy

    return(ramka)
  }

  filmweb_premiera_polska <- function(ramka_dat_premiery){
    if(is.null(ramka_dat_premiery)) return(NA)
    gdzie_polska <- which(ramka_dat_premiery$gdzie=="Polska")
    as.Date(ifelse(length(gdzie_polska)==0, NA,
                   as.character(ramka_dat_premiery$kiedy[gdzie_polska])))
  }

  filmweb_premiera_swiat <- function(ramka_dat_premiery){
    if(is.null(ramka_dat_premiery)) return(NA)
    gdzie_swiat<- which(ramka_dat_premiery$gdzie=="\u015Bwiat")
    as.Date(ifelse(length(gdzie_swiat)==0, NA,
                   as.character(ramka_dat_premiery$kiedy[gdzie_swiat])))
  }

  # box-office:

  filmweb_box_office <- function(html_linku){
    html_linku %>%
      html_nodes(".bottom-15 tr:nth-child(7) td") %>%
      html_text() %>%
      stri_extract_all_regex("[0-9]") %>%
      unlist() %>%
      stri_paste(collapse="") %>%
      as.numeric() -> cena

    if(length(cena)==0) return(NA) else return(cena)
  }

  # world_ranking:

  filmweb_ktory_w_world_rankingu <- function(html_linku){
    html_linku %>%
      html_nodes(".worldRanking") %>%
      html_text() %>%
      stri_match_all_regex("#([0-9].*?) TOP") %>%
      unlist() %>%
      "["(2) %>%
      as.numeric() -> miejsce

    if(length(miejsce)==0) miejsce <- NA

    return(miejsce)
  }

  # obsada i ocena aktorow:

  filmweb_obsada_i_ocena <- function(html_linku){
    html_linku %>%
      html_nodes(".filmCastCast td:nth-child(2) a") %>%
      html_text() -> obsada

    if(length(obsada)==0) return(NA)

    html_linku %>%
      html_nodes(".icon-btn-left , .maxlines-4 a, .s-20:nth-child(2)") %>%
      html_text() %>%
      "["(-1) %>%
      matrix(ncol=2, byrow=TRUE) %>%
      as.data.frame() -> ramka

    colnames(ramka) <- c("aktor", "ocena")

    ramka$ocena %>%
      stri_replace_all_fixed(",", ".") %>%
      unlist() %>%
      as.numeric() -> ramka$ocena

    rbind(ramka,
          data.frame(aktor=obsada[!(obsada %in% ramka$aktor)],
                     ocena=rep(NA, sum(!(obsada %in% ramka$aktor)))))
  }

  # dluzszy opis filmu:

  filmweb_dluzszy_opis_filmu <- function(html_linku){
    html_linku %>%
      html_nodes(".filmMainDescription .text") %>%
      html_text() %>%
      stri_replace_all_fixed("\"", "'") -> tekst

    if(tekst=="Na razie nikt nie doda\u0142 streszczenia fabu\u0142y tego filmu. Mo\u017Cesz by\u0107 pierwszy! Zamie\u015B\u0107 sw\u00F3j opis.") return(NA) else return(tekst)
  }

  # liczba komentarzy:

  filmweb_liczba_komentarzy <- function(html_linku){
    html_linku %>%
      html_nodes(".inline.s-20") %>%
      html_text() %>%
      stri_match_all_regex("[(]([0-9].*?)[)]") %>%
      unlist() %>%
      "["(2) %>%
      stri_extract_all_regex("[0-9]") %>%
      unlist() %>%
      stri_paste(collapse="") %>%
      as.numeric() -> liczba

    if(length(liczba)==0) return(0) else return(liczba)
  }

  # WLASCIWA FUNKCJA:

  tabela_danych <- fread(sciezka_do_tabeli_danych, sep = ",", header = TRUE)

  tabela_danych$portal <- "filmweb"

  tabela_danych$box_office <- NA
  tabela_danych$dluzszy_opis_filmu <- NA
  tabela_danych$gatunek <- NA
  tabela_danych$ile_osob_chce_zobaczyc <- NA
  tabela_danych$kraj_produkcji <- NA
  tabela_danych$krotki_opis_filmu <- NA
  tabela_danych$ktory_w_world_rankingu <- NA
  tabela_danych$liczba_komentarzy <- NA
  tabela_danych$liczba_oddanych_glosow <- NA
  tabela_danych$minuty_trwania_filmu <- NA
  tabela_danych$ocena_filmu <- NA
  tabela_danych$premiera_polska <- NA
  tabela_danych$premiera_swiat <- NA

  n <- nrow(tabela_danych)

  for(i in poczatek:koniec){

    cat(i, "/", koniec, "\n")

    link <- tabela_danych$link_filmweb[i]

    if(is.na(link)) next

    html_linku <- html(link)

    tabela_danych$box_office[i] <- filmweb_box_office(html_linku)
    tabela_danych$dluzszy_opis_filmu[i] <- filmweb_dluzszy_opis_filmu(html_linku)
    tabela_danych$gatunek[i] <- filmweb_gatunek(html_linku)
    tabela_danych$ile_osob_chce_zobaczyc[i] <- filmweb_ile_osob_chce_zobaczyc(html_linku)
    tabela_danych$kraj_produkcji[i] <- filmweb_kraj_produkcji(html_linku)
    tabela_danych$krotki_opis_filmu[i] <- filmweb_krotki_opis_filmu(html_linku)
    tabela_danych$ktory_w_world_rankingu[i] <- filmweb_ktory_w_world_rankingu(html_linku)
    tabela_danych$liczba_komentarzy[i] <- filmweb_liczba_komentarzy(html_linku)
    tabela_danych$liczba_oddanych_glosow[i] <- filmweb_liczba_oddanych_glosow(html_linku)
    tabela_danych$minuty_trwania_filmu[i] <- filmweb_minuty_trwania_filmu(html_linku)
    tabela_danych$ocena_filmu[i] <- filmweb_ocena_filmu(html_linku)
    ramka <- filmweb_tabela_dat_premiery(html_linku)
    tabela_danych$premiera_polska[i] <- filmweb_premiera_polska(ramka)
    tabela_danych$premiera_swiat[i] <- filmweb_premiera_swiat(ramka)

    if(i==poczatek & czy_dopisac_do_pliku==FALSE){
      write.table(tabela_danych[i,],
                  gdzie_zapisac_tabele,
                  col.names = TRUE, row.names = FALSE,
                  quote = TRUE, sep = ",")
    } else{
      if(i==poczatek & czy_dopisac_do_pliku==TRUE){
        write.table(tabela_danych[i,],
                    gdzie_zapisac_tabele,
                    row.names = FALSE, col.names = FALSE, append = TRUE,
                    quote = TRUE, sep = ",")
      } else{
        write.table(tabela_danych[i,],
                    gdzie_zapisac_tabele,
                    col.names = FALSE, row.names = FALSE,
                    quote = TRUE, sep = ",", append = TRUE)
      }
    }

    obsada_i_ocena <- filmweb_obsada_i_ocena(html_linku)
    gdzie <- stri_paste("//", uprosc_tytul(tabela_danych$tytul_filmweb[i]), "_", tabela_danych$rok_produkcji[i], "_ocenaobsady.txt", collapse="")
    write.table(obsada_i_ocena,
                stri_paste(gdzie_zapisac_obsade, gdzie, collapse=""),
                col.names = TRUE, row.names = FALSE,
                quote = FALSE, sep = ",")
  }

}

