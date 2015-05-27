#' Collect info about summary
#'
#' Function \code{summary} gets info about actors.
#'
#' @param links A link - see full summary from imdb.
#' @param titles Titles references from imdb.
#' @param film Either a complete document (XMLInternalDocument), a list of tags (XMLNodeSet) or a single tag (XMLInternalElementNode)
#' @return Character vector
#' @import rvest
#' @import stringi
#' @import XML
#' 

summary = function(links,titles,film){
  wek_pom = stri_detect_regex(titles,'See full summary')
  ktory = which(wek_pom==TRUE)
  if (length(ktory) > 0 ){
    w=links[wek_pom]
    link_opis = stri_paste('http://www.imdb.com',w[1])
    summary = html(link_opis)
    summary = html_nodes(summary,'.plotSummary')
    opis = html_text(summary)
    opis = stri_replace_all_regex(opis,'\\p{WHITE_SPACE}',' ')
    if (is.na(opis)) opis='NA'
  } else {
    opis = html_nodes(film,'#overview-top p')
    opis = html_text(opis)
    opis = stri_replace_all_regex(opis,'\\p{WHITE_SPACE}',' ')
    if (is.na(opis)) opis='NA'
  }
  if (length(opis)==0) opis="NA"
  return(opis)
}