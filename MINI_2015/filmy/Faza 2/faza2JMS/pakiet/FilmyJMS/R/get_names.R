#' Collect names of producers and musicians
#'
#' Function \code{get_names} gets names of director and producers.
#'
#' @aliases subtitle
#' @param www A link - see full cast from imdb.
#' @return a list: first element - musicians, second - producers
#' @import rvest
#'  XML
#'  stringi
#' 
#' 
get_names <- function(www) {
  
  #jesli brak neta lub bledny link - obsluga bledu
  url <- tryCatch({      
    html(www)   
  }, error = function(e) {"NA"})
  
  if(is.character(url)&&url=="NA") return("NA")
  # PRODUCENCI
  
  prod <- tryCatch({
    url %>% html_nodes(".simpleCreditsTable:nth-child(9) a") %>% html_text() %>% 
      stri_trim_both()
  }, error = function(e) {
    "NA"
  })
  
  # MUZYCY
  
  music <- tryCatch({
    url %>% html_nodes(".simpleCreditsTable:nth-child(11) a") %>% html_text() %>% 
      stri_trim_both()
  }, error = function(e) {
    "NA"
  })
  
  #zwraca liste 
  if (length(prod) == 0) 
    prod <- "NA"
  if (length(music) == 0) 
    music <- "NA"
  return(list(music = music, producers = prod))
} 
