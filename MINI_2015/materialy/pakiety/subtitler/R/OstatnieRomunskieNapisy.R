#' Ostatnio dodane napisy w jezyku romunskim.
#'
#'
#' Funkcja \code{OstatnieRomunskieNapisy()} patrzy na stronie ostatnio dodane napisy do romunskich filmow i w zaleznosci od argumentu fileSave zapisuje je do pliku. Kazdy tytul filmu jest zapisywany z rokiem jego premiery.
#'
#'
#' @param imdbGrade Zmienna ilosciowa, selekcjonujaca filmy ktore maja ocene <z portalu Imdb.com> nie nizsza niz podana w argumencie.
#' @param hd Zmienna logiczna, jesli TRUE to zwraca napisy tylko dla filmow 'w duzej rozdzielczosci', domyslnie FALSE.
#' @param fileSave Zmienna logiczna, jesli TRUE to zapisuje do pliku 'OstatnieRomunskieNapisy.txt' w working directory.
#' @param movieYear Zmienna ilosciowa wybierajaca filmy ktore zostaly wyprodukowane w danym roku.
#' @author Pawel Grabowski
#'
#'
#' @examples
#' OstatnieRumunskieNapisy(2011, 9.0, FALSE, TRUE)
#' OstatnieRumunskieNapisy(movieYear = 2001, imdbGrade = 8, fileSave = TRUE, hd = TRUE)
#'
#'
#'
#' @import stringi
#' @import dplyr
#' @import rvest


OstatnieRomunskieNapisy <- function(movieYear = 2015, imdbGrade = 5.0, fileSave = FALSE, hd = FALSE) {
  if(is.logical(fileSave) == FALSE) {
    stop("Zmienna fileSave musi byc typu logicznego")
  }
  if(is.logical(hd) == FALSE) {
    stop("Zmienna hd musi byc typu logicznego")
  }
  if(is.logical(fileSave) == FALSE) {
    stop("Zmienna fileSave musi byc typu logicznego")
  }
  if(is.numeric(movieYear) == FALSE){
    stop("Zmienna movieYear musi byc typu numeric")
  }
  if(is.numeric(imdbGrade) == FALSE) {
    stop("Zmienna imdbGrade musi byc typu numeric")
  }
  urlTemp <- paste0("http://www.opensubtitles.org/pl/search/sublanguageid-all/searchonlymovies-on/movieimdbratingsign-5/movieimdbrating-",
    imdbGrade, "/movieyearsign-1/movieyear-", movieYear, "/")
  if (hd == TRUE) {
    htmlSite <- html(paste0(urlTemp, "hd-on"))
  } else {
    htmlSite <- html(urlTemp)
  }
  moviesNames <-  html_nodes(htmlSite, ".bnone") %>%
    html_text() %>%
    stri_replace_all_regex(.,"\n \t\t\t|;", "")
  moviesDownloadLinks <- html_nodes(htmlSite, "#search_results td:nth-child(5) a") %>%
    html_attr("href") %>%
    stri_paste("http://www.opensubtitles.org",.)
  dfTemp <- data.frame(Nazwa = moviesNames, Link = moviesDownloadLinks)
  dfTemp <- dfTemp[!duplicated(dfTemp[,1]),]
  if (fileSave == TRUE) {
    f <- file(stri_paste(getwd(), "/", 'OstatnieRomunskieNapisy.txt'), open = 'a')
    writeLines("Nazwa;Format;link", f)
    for(i in 1: dim(dfTemp)[1]) {
      writeLines(stri_paste(dfTemp[i,1], dfTemp[i,2], sep = ";"), f)
    }
    close(f)
  }
  return(dfTemp)
}

