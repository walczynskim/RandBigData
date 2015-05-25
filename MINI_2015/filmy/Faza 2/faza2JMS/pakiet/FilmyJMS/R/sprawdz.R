#' Checks whether the value is NA and converts it to a string.
#'
#' Function \code{wyjmij} checks whether the value is NA and converts it to a string.
#'
#' @param cecha Vector
#' @return Character vector
#' 

sprawdz = function(cecha){
    if (is.na(cecha)){
      cecha='NA'  
    }else return(cecha)
}
