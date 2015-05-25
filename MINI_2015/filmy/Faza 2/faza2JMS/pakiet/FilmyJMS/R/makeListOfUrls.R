#' Funkcja pobiera linki do filmów z bazy IMdB
#'
#' @param urlStart, początkowy url
#'
#' @examples
#' # ściągamy filmy z kilku list - MovieMeter, Numer of Votes, US Box Office
#' # dzięki temu będziemy mieć znacznie więcej niż 100 000, bo filmy są w różnej kolejności
#' # więc parsujemy trzy podstrony i bierzemy niepowtarzające się filmy
#'
#' tab <- makeListOfUrls('http://www.imdb.com/search/title?at=0&sort=moviemeter,asc&start=1&title_type=feature')
#' write.table(tab, 'linki_tmp.txt', row.names=FALSE, col.names=FALSE)
#'
#' tab2 <- makeListOfUrls('http://www.imdb.com/search/title?at=0&sort=num_votes&start=1&title_type=feature')
#' write.table(tab6, 'linki_tmp2.txt', row.names=FALSE, col.names=FALSE)
#'
#' tab3 <- makeListOfUrls('http://www.imdb.com/search/title?at=0&sort=boxoffice_gross_us&start=1&title_type=feature')
#' write.table(tab3, 'linki_tmp3.txt', row.names=FALSE, col.names=FALSE)
#'
#' @import rvest
#' @import stringi
#' @import XML
#'
# w bazie na dzień 16.04 jest 318,485 filmów wyświetlane są po 50 na stronie
# ściągamy pierwsze 100 000, bo linki są analogiczne potem do linków
# dodawane są tokeny, który uniemożliwiają parsowanie strony
makeListOfUrls <- function(urlStart) {
    howManyPages <- round(318485/50)
    linksList <- list()
    
    for (i in 1:howManyPages) {
        j <- 50 * (i - 1) + 1
        print(j)
        startNew <- paste("start=", j, sep = "")
        urlNew <- stri_replace_all_regex(urlStart, "start=1", startNew)
        titleLinks <- getLinks(urlNew)
        linksList[[i]] <- makeLongPath(titleLinks)
    }
    vector <- combineList(linksList)
    return(vector)
}

getLinks <- function(url) {
    allLinks <- getHTMLLinks(url, xpQuery = "//@href")
    titleLinks <- allLinks[stri_detect_regex(allLinks, "^/title/tt[0-9]+/$")]
    titleLinks <- titleLinks[!duplicated(titleLinks)]
    return(titleLinks)
}

makeLongPath <- function(links) {
    longPaths <- paste("http://www.imdb.com", links, sep = "")
    return(longPaths)
}

combineList <- function(UrlList) {
    n <- length(UrlList)
    if (n == 1) {
        return(UrlList)
    } else {
        tmpV <- UrlList[[1]]
        for (i in 2:n) {
            cV <- c(tmpV, UrlList[[i]])
            tmpV <- cV
        }
        return(tmpV)
    }
}

# tabela <- c(tab, tab2, tab3) tabela <- tabela[!duplicated(tabela)]

# write.table(tabela, 'linki_filmy.txt', row.names=FALSE, col.names=FALSE) 
