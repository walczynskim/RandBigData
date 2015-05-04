#' Robienie wykresu sentymentu
#'
#' @param tytul zmienna typu character, case-insensitive, wskazujaca napisy jakiego filmu nas interesuja.
#' Tytuly filmow musza byc w jezyku angielskim.
#'
#' @author Katarzyna Fak, Marcni Kosinski, Marcin Rdzanowski
#'
#' @examples
#' sentyment_dla_filmu("Batman")
#' sentyment_dla_filmu("Titanic")
#' sentyment_dla_filmu("Star Wars")
#' @import rvest
#' @import stringi
#' @export

sentyment_dla_filmu <- function( tytul ){
   x <- pobierz_napisy_pierwsze_mozliwe( tytul ,"eng")
   napisy <- readLines( paste0(x, "/", grep(".srt", list.files( x), value = TRUE)) )
   napisy2 <- stri_extract_all_words(napisy)
   napisy2 <- napisy[stri_length(napisy) > 4]
   napisy3 <- napisy2[!grepl("-->", napisy2)]

   
   negative <- readLines("https://raw.githubusercontent.com/pbiecek/RandBigData/master/MINI_2015/materialy/webscrap/negative-words.txt")
   positive <- readLines("https://raw.githubusercontent.com/pbiecek/RandBigData/master/MINI_2015/materialy/webscrap/positive-words.txt")
   positive <- positive[-c(1:36)]
   negative <- negative[-c(1:35)]

   sapply( napisy3, function( element ){
      sum( unlist(stri_extract_all_words(element)) %in% positive)-
         sum( unlist(stri_extract_all_words(element)) %in% negative)
   }) -> sentyment
   cumsum(sentyment) -> sentyment
   plot(sentyment)
   
   #message("Sentyment zrobiono, szefuniu.")
}