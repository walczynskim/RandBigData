#' Pobierz charakterysyki filmow z Wikipedii.
#'
#' Funkcja \code{wikipedia_charakterystyki} szuka informacji o filmach na Wikipedii.
#'
#' @aliases wikipedia_charakterystyki
#' @param filmy_wiki Ramki danych o kolumnach z nazwami "tytul_wikipedia", "link_wikipedia", "rok_produkcji". Taka ramke danych mozna otrzymac korzystajac z funkcji \code{wikipedia_tytul_rok_link}.
#' @param sciezka_wyjscie Sciezka do pliku tekstowego, w ktorym chcemy zapisac pobrane dane.
#' @return Funkcja \code{wikipedia_charakterystyki} zwraca ramke danych o 10 kolumnach zawierajaca informacje o danym filmie znajdujace sie na Wikipedii.
#' @details Kolumna "link_wikipedia" argumentu \code{filmy_wiki} powinna zawierac linki do filmow z Wikipedii. Wsrod informacji otrzymanych przy pomocy funkcji \code{wikipedia_charakterystyki} znajdujemy: "tytul_wikipedia", "link_wikipedia", "gatunek", "rok_produkcji", "kraj_produkcji", "jezyk", "czas_trwania", "rezyseria", "budzet", "waluta".


wikipedia_charakterystyki <- function(filmy_wiki, sciezka_wyjscie){
  if(!is.data.frame(filmy_wiki)){
    stop("Argument filmy_wiki powinien byc ramka danych.")
  }
  if(!all(c("tytul_wikipedia", "link_wikipedia", "rok_produkcji") %in% colnames(filmy_wiki))){
    stop("Argument filmy_wiki powinien miec kolumny o nazwach 'tytul_wikipedia', 'link_wikipedia', 'rok_produkcji'.")
  }
  info <- data.frame(tytul = character(0), link = character(0),
                     gatunek = character(0), rok_produkcji = character(0),
                     kraj_produkcji = character(0), jezyk = character(0),
                     czas_trwania = character(0), rezyseria = character(0),
                     budzet = character(0), stringsAsFactors = FALSE)
  if(nrow(filmy_wiki) == 0){return(info)}

  jakie_info <- c("Gatunek", "Kraj produkcji", "J\u0119zyk",
                  "Czas trwania", "Re\u017Cyseria", "Bud\u017Cet")

  for(i in 1:nrow(filmy_wiki)){
    strona_film <- html(filmy_wiki$link[i])
    tabela <- html_nodes(strona_film, ".infobox td")
    tabela_tekst <- html_text(tabela)[-1]

    w <- match(jakie_info, tabela_tekst)

    info <- rbind(info, data.frame(tytul = filmy_wiki$tytul[i],
                                   link = filmy_wiki$link[i],
                                   gatunek = ifelse(is.na(w[1]), NA, tabela_tekst[w[1]+1]),
                                   rok_produkcji = filmy_wiki$rok_produkcji[i],
                                   kraj_produkcji = ifelse(is.na(w[2]), NA, tabela_tekst[w[2]+1]),
                                   jezyk = ifelse(is.na(w[3]), NA, tabela_tekst[w[3]+1]),
                                   czas_trwania = ifelse(is.na(w[4]), NA, tabela_tekst[w[4]+1]),
                                   rezyseria = ifelse(is.na(w[5]), NA, tabela_tekst[w[5]+1]),
                                   budzet = ifelse(is.na(w[6]), NA, tabela_tekst[w[6]+1]),
                                   stringsAsFactors = FALSE))
  }

  # czyszcze dane z niesensownych dopiskow
  info$gatunek <- stri_replace_all_charclass(info$gatunek, "\\p{WHITE_SPACE}", " ")
  info$kraj_produkcji <- stri_replace_all_charclass(info$kraj_produkcji, "\\p{WHITE_SPACE}", " ")
  info$jezyk <- stri_trans_tolower(info$jezyk)
  info$jezyk <- stri_replace_all_charclass(info$jezyk, "\\p{WHITE_SPACE}", " ")

  # zamieniam czas trwania filmow na minuty
  czas <- stri_replace_all_regex(info$czas_trwania, "\\[.+\\]", "")
  czas <- stri_extract_all_regex(czas, "[0-9]+")
  czas <- sapply(czas, function(x){if(length(x) == 1){return(as.numeric(x))}
                                   else if(length(x) == 2){return(60*as.numeric(x[1])+as.numeric(x[2]))}
                                   else{return(NA)}})
  info$czas_trwania <- czas

  bud <- stri_replace_all_regex(info$budzet, "\\[.+\\]", "")
  n_a <- is.na(bud)
  waluta <- character(length(bud))
  bud2 <- stri_extract_all_regex(bud, "[0-9]+")
  bud2 <- sapply(bud2, function(x){stri_paste(x, collapse = "")})
  bud2 <- as.numeric(bud2)
  mln1 <- stri_detect_fixed(bud, "mln")
  mln2 <- stri_detect_fixed(bud, "mil")
  mln <- mln1 | mln2
  bud2 <- ifelse(mln, bud2*1000000, bud2)
  info$budzet <- bud2

  dolar1 <- stri_detect_fixed(bud, "$")
  dolar2 <- stri_detect_fixed(bud, "USD")
  dolar <- dolar1 | dolar2
  dolar[is.na(dolar)]  <- FALSE
  waluta[dolar] <- "$"
  euro <- stri_detect_fixed(bud, "\u20AC")
  euro[is.na(euro)] <- FALSE
  waluta[euro] <- "\u20AC"
  waluta[n_a] <- NA
  wal_nar <- !(dolar | euro | n_a)
  waluta[wal_nar] <- "waluta narodowa"
  info$waluta <- waluta

  info$tytul <- stri_replace_all_regex(info$tytul, "\"", "'")
  info$gatunek <- stri_replace_all_regex(info$gatunek, "\"", "'")
  info$rok_produkcji <- stri_replace_all_regex(info$rok_produkcji, "\"", "'")
  info$kraj_produkcji <- stri_replace_all_regex(info$kraj_produkcji, "\"", "'")
  info$jezyk <- stri_replace_all_regex(info$jezyk, "\"", "'")
  info$czas_trwania <- stri_replace_all_regex(info$czas_trwania, "\"", "'")
  info$rezyseria <- stri_replace_all_regex(info$rezyseria, "\"", "'")
  info$budzet <- stri_replace_all_regex(info$budzet, "\"", "'")

  write.table(info, sciezka_wyjscie, quote = TRUE, sep = ",",
              row.names = FALSE, col.names = TRUE)
}


