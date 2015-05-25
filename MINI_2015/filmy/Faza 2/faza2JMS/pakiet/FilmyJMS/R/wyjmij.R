#' Collect any information.
#'
#' Function \code{wyjmij} gets info about something .
#'
#' @param film Either a complete document (XMLInternalDocument), a list of tags (XMLNodeSet) or a single tag (XMLInternalElementNode)
#' @param nodes Nodes to select 
#' @return Character vector
#' @import rvest
#' @import stringi
#' @import XML
#' 

wyjmij = function(film, nodes){
  wez = html_nodes(film,nodes)
  nazwa = html_text(wez)
  nazwa = stri_replace_all_regex(nazwa,'\\p{WHITE_SPACE}',' ')
  if (length(nazwa)==0 ){
    return("NA")
  } else {
     return(nazwa)
  }
}