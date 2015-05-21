#' Funkcja sprawdzajaca czy jakies wartosci pewnej cechy filmu powtarzaja sie.
#'
#' Funkcja \code{czy_zawiera} sprawdza czy elementej jednej cechy filmu zawiera sie 
#' w tej samej cesze drugiego flimu np. czy kraje powstania filmu sie powtarzaja.
#' 
#' @usage czy_zawiera(glowny, por)
#' @param glowny napis okreslajacy ceche filmu glownego, ktorego separatorem jest @
#' @param por napis okreslajacy ceche filmu porownawczego, ktorego separatorem jest @
#'
#' @return
#' wartosc numeryczna 0 lub 1.
#' 
#' @example
#' czy_zawiera("Poland@USA","USA")
#'
#'@author Adrianna Sudol
#'
#'
czy_zawiera = function(glowny,por){
  
  if (glowny=="NA"||por=="NA"||is.na(glowny)||is.na(por)){
    return(NA)
  }
  
  
  glowny = unlist(stri_split_regex(glowny,'@'))
  por = unlist(stri_split_regex(por,'@'))
  czy = vector()
  for ( i in seq_along(glowny)){
    pom = stri_detect_fixed(glowny[i],por)
    wynik = c(czy,pom)
  }
  
  if (any(wynik==TRUE)){
    return(1)
  }else return(0)
  
}