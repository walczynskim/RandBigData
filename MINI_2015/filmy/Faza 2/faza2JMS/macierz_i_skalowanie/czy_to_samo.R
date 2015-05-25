#' Funkcja okreslajaca czy dwa filmy otrzymały te same nagrody, czy nie.
#'
#' Funkcja \code{czy_to_samo} okresla czy dwa filmy porownanwcze otrzymaly
#' te same nagrody np. Oscary
#' 
#' @usage czy_to_samo(text_gl, text_por)
#' @param text_gl liczba naturalna okreslajaca liczbe nagrod filmu glownego
#' @param text_por liczba naturalna okreslajaca liczbe nagrod filmu porownawczego
#'
#' @return
#' wartosc numeryczna 0 lub 1.
#' 
#' @example
#' czy_to_samo(3,0)
#'
#'@author Adrianna Sudol
#'


czy_to_samo = function(text_gl,text_por){
  
  if(text_gl=="NA"||text_por=="NA"||is.na(text_gl)||is.na(text_por)){
    return(NA)
  }
  if (text_gl>0 && text_por>0){
    return(1)
  }else{
    return(0)
  }
  
}