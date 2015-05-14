#' Rysowanie kolorowej chmury slow z napisow
#'
#' Funkcja \code{fak_jankowiak} pobiera do folderu napisy do filmow pod wskazanym jezykiem.
#'
#' @param tytul zmienna typu character, case-insensitive, wskazujaca napisy jakiego filmu nas interesuja.
#' Tytuly filmow nie musza byc w oryginalnym jezyku.
#' @param jezyk szukanych napisow.
#' @details Pobieranie pierwszych z brzegu napisow z portalu opensubtitles i rysowanie ich w postaci kolorowej chmury.
#' Funkcja jest poczatkiem czegos wielkiego: mozna rozbudowac ja dowolnie, by uzytkownik wskazal ile/ktore napisy go
#' konkretnie interesuja.
#' @return wdzieczy i kolorowy obrazek
#' @author Justyna Jankowiak & Katarzyna Fak
#'
#' @examples
#' fak_jankowiak("La vita e bella","pol")
#' fak_jankowiak("Madagaskar","heb") # napisy w jezyku hebrajskim
#' @import rvest
#' @import stringi
#' @import tm
#' @import RColorBrewer
#' @import wordcloud
#' @export
fak_jankowiak <- function(tytul,jezyk){
napisy <- pobierz_napisy_do_chmury(tytul,jezyk)
wyczyszczone <- czysc(napisy)
rysuj(wyczyszczone)
}
rysuj <- function(oczyszczone){
pal2 <- brewer.pal(8,"Set2")
wordcloud(oczyszczone, scale=c(5.5, 0.4), max.words=150,
random.order=F, rot.per=.3, colors=pal2)
}
czysc <- function(napisy){
czas <- stri_detect_regex(napisy, "[0-9]{2}:[0-9]{2}:[0-9]{2}[,.][0-9]{1,3}")
ktore.czas <- which(czas)
napisy <- napisy[!czas]
napisy <- napisy[!napisy==""]
napisy <- stri_replace_all_regex(napisy, "[0-9]+", "")
napisy <- stri_replace_all_regex(napisy, "[{}]", "")
ktore.html <- stri_detect_regex(napisy, "^<.+>")
napisy <- napisy[!ktore.html]
napisy <- napisy[!is.na(napisy)]
napisy <- paste(napisy, collapse = " ")
corpus <- Corpus(VectorSource(napisy))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("en"))
dataframe <- data.frame(text=unlist(sapply(corpus, `[`, "content")),
stringsAsFactors=F)
return(as.character(dataframe))
}
