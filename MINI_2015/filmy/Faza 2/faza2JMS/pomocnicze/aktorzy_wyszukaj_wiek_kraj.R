#' Funkcja dla danego filmu, wyszukuje pierwszych 10 aktorow w ramce danych aktorzy.
#'
#' Funkcja \code{aktorzy_wyszukaj_wiek_kraj} dla danego filmu, wyszukuje pierwszych
#' 10 aktorow w ramce danych aktorzy (wyciaga takie informacje jak ich wiek w momencie
#' grania w filmie i kraj pochodzenia)
#' 
#' @usage aktorzy_wyszukaj_wiek_kraj(f, aktorzy)
#' @param f ramka danych z informacja o jednym filmie
#' @param aktorzy ramka danych z aktorami
#'
#' @return
#' lista z informacjami o aktorach, pierwszy element listy to ich wiek w momencie
#' grania w filmie, natomiast drugi element to wektor krajow pochodzenia
#' 
#' @example
#' 
#' aktorzy <- read.csv2("Actors.csv")
#' filmy <- read.csv2("filmy.csv", stringsAsFactors=FALSE)
#' aktorzy_wyszukaj_wiek_kraj(filmy[1,], aktorzy)
# 
#'
#'@author Emilia Momotko
#'
#'@import stringi
#'

aktorzy_wyszukaj_wiek_kraj <- function(f, aktorzy){
  
  aktorzy_f <- unlist(stri_split_fixed(f[1,22],"@"))[1:10]
  
  
  l1 <- aktorzy[aktorzy$name%in%aktorzy_f,]
  l1 <- l1[!duplicated(l1$name),]
  
  
  if(f[4]!="NA"){
    wiek <- 2015-as.numeric(l1[[3]]) - (2015-as.numeric(f[1,4]))
  } else wiek <- NA
  
  kraj <- as.character(l1[[4]])
  
  list(wiek,kraj)
}