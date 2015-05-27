#' Funkcja sprawdza czy dwie wartosci sa odpowiednio sobie bliskie
#'
#' Funkcja \code{porownaj_mediany} sprawdza czy dwie wartosci liczbowe (w domysle sa to
#' mediany wieku) nie roznia sie zbytnio.
#'
#' @usage porownaj_mediany(m1, m2)
#' @param m1 liczba naturalna - mediana pierwszej grupy badanych osob
#' @param m2 liczba naturalna - mediana drugiej grupy badanych osob
#'
#' @return
#' wartosc numeryczna 1, jesli mediany sa zblizone, 0 wpp.
#'
#'@author Emilia Momotko
#'

porownaj_mediany <- function(m1, m2){

  if(is.null(m1)||is.null(m2)||is.na(m1)||is.na(m2)||length(m1)==0||length(m2)==0) return(NA)

  if(m1<6){

    if(m2<6){

      if(abs(m1-m2)<=1) return(1)
    }

  } else if(m1<18){

    if(m2>=6&&m2<18){

      if(abs(m1-m2)<=3) return(1)
    }
  } else if(m1<50){

    if(m2>=18&&m2<50){

      if(abs(m1-m2)<=4) return(1)
    }
  } else if(m1>=50){

    if(m2>=50){

      if(abs(m1-m2)<=7) return(1)
    }
  }

  return(0)

}
