#' Collect any information.
#'
#' Function \code{wydobadz} gets info about something .
#'
#' @param links A link - see full summary from imdb.
#' @param titles Titles references from imdb.
#' @param film Either a complete document (XMLInternalDocument), a list of tags (XMLNodeSet) or a single tag (XMLInternalElementNode)
#' @param nodes1 Nodes to select 
#' @param nodes2 Nodes to select
#' @return Character vector
#' @import rvest
#' @import stringi
#' @import XML
#' 

wydobadz = function(links,titles,film,nodes1,nodes2){
  wek_pom = stri_detect_regex(titles,nodes1)
  ktory = which(wek_pom==TRUE)
  wynik=vector()
  if (length(ktory) > 0 ){
    w=links[wek_pom]
    link_n = stri_paste('http://www.imdb.com',w[1])
    awards = html(link_n)
    awards = html_nodes(awards,nodes2)
    wynik = html_text(awards)
    wynik = stri_replace_all_regex(wynik,'\\p{WHITE_SPACE}',' ')
   
  }
  if (length(wynik)==0){
    return("NA")
  } else {
    return(wynik)
  }
}