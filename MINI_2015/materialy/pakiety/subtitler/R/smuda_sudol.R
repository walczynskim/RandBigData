#' Pobieranie i wyznaczenie czestosci liter w napisach
#'
#' Funkcja \code{smuda_sudol} pobiera do folderu napisy do filmu ze wskazanym jezykiem oraz wyznacza czestosc
#' liter dla tych napisow
#'
#' @param film zmienna typu character, case-insensitive, wskazujaca napisy jakiego filmu nas interesuja.
#' Tytuly filmow nie musza byc w oryginalnym jezyku.
#' @param jezyk szukanych napisow.
#'
#' @details
#' Funkcja pobiera napisy do filmu przy pomocy funkcji \code{pobierz_napisy}, a nastepnie wczytuje
#' napisy z folderu z napisami i zlicza czestosc uzytych liter w tych napisach.
#'
#' @return Zwraca wektor nazwany z liczba wystapien danych liter z nazw.
#'
#' @author Adrianna Sudol, Piotr Smuda
#'
#' @examples
#' smuda_sudol("Titanic","pol")

smuda_sudol<-function(film,jezyk){
  pobrane<-pobierz_napisy(film,jezyk)
  sciezka<-list.files(pobrane,recursive = TRUE, full.names = TRUE)
  sciezka<-sciezka[stri_detect_regex(sciezka,'(\\.srt$|\\.txt$)')]
  napisy<-readLines(sciezka)
  litery<-stri_extract_all_regex(napisy,'\\p{L}')
  litery<-stri_trans_tolower(unlist(litery))
  litery<-litery[!(is.na(litery))]
  wynik<-sort(table(litery),decreasing = TRUE)
  return(wynik)
}
