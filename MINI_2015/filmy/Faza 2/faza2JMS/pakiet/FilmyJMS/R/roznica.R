#' Funkcja okreslajaca cechy filmow zawierają się w oczekiwanym przedziale wartosci
#'
#' Funkcja \code{roznica} okresla czy pewna cecha dla dwoch filmow jest podobna pod
#' wzgledem zawierania sie w podobnych przedzialach. Sluzy do porownania ocen filmu,
#' liczby uzytkownikow oceniajacych oraz liczby recenzji.
#'
#' @usage roznica(glowny, por,param)
#' @param glowny liczba naturalna (jako wektor napisow) okreslajaca ceche filmu glownego
#' @param por liczba naturalna (jako wektor napisow) okreslajaca ceche filmu porownawczego
#' @param param wartosc liczbowa okreslajaca max jakim moga sie roznic dane cechy, dla
#'        oceny bedzie 0.3, liczby uzytkownikow 50000, liczby recenzji 20
#'
#' @return
#' wartosc numeryczna 0 lub 1.
#'
#'
#'@author Adrianna Sudol
#'
#'
roznica = function(glowny,por,param){

  glowny = as.numeric(glowny)
  por = as.numeric(por)
  if(glowny=="NA"||por=="NA"||is.na(glowny)||is.na(por)) return(NA)

  if (abs(glowny-por) <= param){
    return(1)
  }else return(0)

}
