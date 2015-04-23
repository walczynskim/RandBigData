#' Polacz tabele z filmami
#'
#' Funkcja \code{polacz_tabele_z_filmami()} laczy wszystkie tabele po kluczu: \code{tytul} i \code{rok_produkcji} w jedna.
#'
#' @param sciezka_do_tabel wektor napisow, bedacy sciezka dostepu do tabel, ktore chcemy polaczyc (kolumny tabel powinny miec nazwy: \code{tytul_portal}, \code{rok_produkcji}, gdzie za \code{portal} trzeba wstawic odpowiednia nazwe portalu).
#' @param sciezka_do_pliku_txt_gdzie_zapisac sciezka do pliku .txt, w ktorym chcemy zapisac nasza ramke danych
#' @return Zwracana jest ramka danych o kolumnach z ramek wejsciowych oraz dodatkowej kolumnie \code{tytul_uproszczony}. Kolumny \code{tytul_uproszczony} i \code{rok_produkcji} stanowia klucz glowny w tej ramce danych.

polacz_tabele_z_filmami <- function(sciezka_do_tabel, sciezka_do_pliku_txt_gdzie_zapisac){

  ile <- length(sciezka_do_tabel)
  lista_tabel <- vector("list", ile)

  for(i in 1:ile){

    sciezka_do_tabel[i] %>%
      fread(header = TRUE, sep = ",") %>%
      unique() -> tabela

    tabela$tytul_uproszczony <- uprosc_tytul(tabela$tytul)
    lista_tabel[[i]] <- tabela

  }

  tabela <- lista_tabel[[1]]
  for(i in 2:ile){
    tabela <- full_join(tabela, lista_tabel[[i]], by = c("tytul_uproszczony", "rok_produkcji"))
  }

  write.table(tabela, sciezka_do_pliku_txt_gdzie_zapisac, quote = TRUE, sep = ",",
              row.names = FALSE, col.names = TRUE)

}

