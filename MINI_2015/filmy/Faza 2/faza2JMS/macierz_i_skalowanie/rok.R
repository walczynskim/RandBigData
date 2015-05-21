#' Funkcja okreslajaca czy lata powstania fillmu sa zblizone.
#'
#' Funkcja \code{rok} okresla czy dwa porwnawcze filmy sa podobne pod wzgledem lat
#' powstania. Uwzgledniony jest fakt ze roznica miedzy rokiem 1942 a 1948 jest inna niz
#' miedzy 2005 a 2011, gdyz wiadomo postep techniki byl inny.
#' 
#' @usage rok(rok_glowny, rok_por)
#' @param rok_glowny liczba naturalna (jako wektor napisow) okreslajaca rok wydania filmu glownego
#' @param por liczba naturalna (jako wektor napisow) okreslajaca rok wydania filmu porownawczego
#'
#'
#' @return
#' wartosc numeryczna 0 lub 1.
#' 
#' @example
#' rok("1945","1974")
#'
#'@author Adrianna Sudol
#'
#'

rok = function(rok_glowny,rok_por){
  
  if (rok_glowny=="NA"||rok_por=="NA"){
    return(NA)
  }
  rok_glowny = as.numeric(rok_glowny)
  rok_por = as.numeric(rok_por)
  if (rok_glowny < 1920){
    if(rok_por <1920){
      return(1)
    }else {return(0)}
  }else if(rok_glowny>1920 && rok_glowny<2000){
    if (rok_por<2000 && rok_por>1920){
      roznica = abs(rok_glowny-rok_por)
      if (roznica <= 10){
        return(1)
      }else{
        return(0)
      }
    }else{
      return(0)
    }
  }else if(rok_glowny > 2000){
    if(rok_por > 2000){
      roznica = abs(rok_glowny-rok_por)
      if (roznica <= 3){
        return(1)
      }else{
        return(0)
      }
    }else return(0)
  }
  
}