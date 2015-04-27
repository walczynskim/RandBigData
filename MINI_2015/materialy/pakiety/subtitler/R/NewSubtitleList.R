#' Information about newest subtitles on opensubtitles.org
#'
#' Function \code{NewSubtitleList} return data.frame with information about newest subtitles
#' on opensubtitles.org
#'
#' @param language character vector, the language, in which subtitles will
#' be searched
#'
#'
#' @examples
#' NewSubtitleList()
#' NewSubtitleList('eng')
#' NewSubtitleList('pol')
#'
#'
#' @author Mikolaj Wasniewski
#'
#' @import rvest
#' @import stringi
#'






NewSubtitleList <- function(language="all"){
  stopifnot(is.vector(language), is.character(language), length(language)==1,
            stri_length(language)==3)
  language<-stri_trans_tolower(language)
  url<-stri_paste("http://www.opensubtitles.org/pl/search/sublanguageid-",
                  language)
  html<-html(url)
  title <- html_nodes(html, ".bnone") %>% html_text() %>%
    stri_replace_all_regex("\n|\t","")

  if (length(title)>0){
    link <- html_nodes(html, "#search_results td:nth-child(5) a") %>%
      html_attr("href") %>% stri_paste("http://www.opensubtitles.org",.)

    date <- html_nodes(html, "time") %>% html_text()

    language=html_nodes(html, ".expandable .flag") %>% html_attr("class") %>%
      stri_replace_all_regex("flag ", "")

    return(data.frame(title=title, language=language, date=date, link=link))
  } else {
    return("not found any subtitles")
  }
}













