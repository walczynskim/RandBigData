#' Funkcja tworzy macierz podobienstw
#'
#' Funkcja \code{tworz_macierz} tworzy macierz podobienstw na podstawie zadanej 
#' liczby filmow do przeanalizowania i ramki danych z filmami
#' 
#' @usage tworz_macierz(ile, filmy)
#' @param ile liczba naturalna, dodatnia, liczba porownywanych filmow
#' @param filmy ramka danych z filmami
#'
#' @return
#' macierz rozmiaru ile x ile
#' 
#' @example
#' 
#' filmy <- read.csv2("filmy24.csv",stringsAsFactors=FALSE)
#' tworz_macierz(100, filmy)
#'
#'@author Emilia Momotko
#'

tworz_macierz <- function(ile, filmy){
  
  m <- matrix(0,ncol=ile, nrow=ile)
  
  for(i in 1:(ile-1)){
    
    for(j in (i+1):ile)
      m[i,j] <- odleglosc(filmy[i,],filmy[j,], filmy) 
    
  }
  
  p <- m+t(m)
  diag(p)<-1
  colnames(p)<- filmy[1:ile,1]
  rownames(p)<- filmy[1:ile,1]
  p
  
}