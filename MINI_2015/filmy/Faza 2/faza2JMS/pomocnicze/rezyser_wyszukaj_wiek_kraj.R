#' Funkcja dla danego filmu, wyszukuje pierwszych rezyserow w ramce danych rezyserzy
#'
#' Funkcja \code{rezyser_wyszukaj_wiek_kraj} dla danego filmu, wyszukuje rezyserow w ramce
#' danych rezyserzy (wyciaga takie informacje jak ich wiek w momencie
#' rezyserowania filmu i kraj pochodzenia)
#' 
#' @usage rezyser_wyszukaj_wiek_kraj(f, rezyserzy)
#' @param f ramka danych z informacja o jednym filmie
#' @param rezyserzy ramka danych z rezyserami
#'
#' @return
#' lista z informacjami o rezyserze lub rezyserach: pierwszy element listy to ich wiek w momencie
#' grania w filmie, natomiast drugi element to wektor krajow pochodzenia
#' 
#' @example
#' 
#' rezyserzy <- read.csv2("Directors.csv")
#' filmy <- read.csv2("filmy.csv", stringsAsFactors=FALSE)
#' aktorzy_wyszukaj_wiek_kraj(filmy[1,], rezyserzy)
# 
#'
#'@author Emilia Momotko
#'
#'@import stringi
#'

rezyser_wyszukaj_wiek_kraj <- function(f,rezyserzy){
  
  rezyserzy_f <- unlist(stri_split_fixed(f[1,23],"@"))
  
  l1 <- rezyserzy[rezyserzy$name%in%rezyserzy_f,]
  l1 <- l1[!duplicated(l1$name),]
  
  
  if(f[4]!="NA"){
    wiek <- 2015-as.numeric(l1[[3]]) - (2015-as.numeric(f[1,4]))
  } else wiek <- NA
  
  kraj <- as.character(l1[[4]])
  
  list(wiek,kraj)
  
}