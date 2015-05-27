#' Collect info about directors
#'
#' Function \code{director} gets info about directors.
#'
#' @aliases subtitle
#' @param www A link - see full cast from imdb.
#' @return list with info about actors, moreover all info is saved in csv file.
#' @import rvest
#'  XML
#' 
#' 

director <- function(www) {
    
  #selektor do wydobycia danych
  selector <- c(director="//td[@class='name']//a")
  
  #jesli brak internetu lub linku - obsluga bledu
  htmls <- tryCatch({
    htmls_movie(www, selector)
  }, error = function(e) {"NA"})
  
  #jesli byl blad to nic nie robi
  if (length(htmls) == 1 && is.character(htmls) && htmls == "NA") 
    return("NA")
  
  #zbiera info
  lista <- harvest_people(htmls)
  
  #zapisuje
  create("Director.csv",lista)
  
  #zwraca liste
  return(lista$name)   
} 
