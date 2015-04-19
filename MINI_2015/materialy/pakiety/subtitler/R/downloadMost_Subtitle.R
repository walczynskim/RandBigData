#' Function downloads the newest, the most recommended or the most popular subtitles from opensubtitles.org
#'
#' Function creates folder with downloaded subtitles (as .zip file or unzipped .txt or .srt file)
#'
#' @param which, character one of `popular`, `new` or `recommended`
#' @param unzip, logical, if TRUE the .zip file is unzipped
#' @param info, logical, if TRUE the file with information is unzipped, otherwise it is deleted
#'
#' @examples
#' downloadMost_Subtitle()
#' downloadMost_Subtitle('new')
#' downloadMost_Subtitle(unzip=FALSE)
#' downloadMost_Subtitle('recommended', unzip=TRUE, info=TRUE)
#'
#' @author Justyna Jankowiak
#'
#' @import rvest
#' @import stringi
#'

downloadMost_Subtitle <- function(which = "popular", unzip = TRUE, info = FALSE) {
    if (!is.logical(unzip) | !is.logical(info))
        stop("arguments 'unzip' and 'info' must be logical")
    if (info == TRUE & unzip == FALSE)
        stop("'info' can be TRUE only if 'unzip' is TRUE")
    if (!(which %in% c("popular", "new", "recommended")))
        stop("wrong 'which' argument")

    if (which == "popular")
        html <- html("http://www.opensubtitles.org/en/search/sublanguageid-all/subadddate-3/sort-7/asc-0")
    if (which == "new")
        html <- html("http://www.opensubtitles.org/pl/search/sublanguageid-pol")
    if (which == "recommended")
        html <- html("http://www.opensubtitles.org/pl/search/sublanguageid-pol/subfeatured-on")

    nodes <- html_nodes(html, "#search_results td:nth-child(5) a")
    href <- html_attr(nodes, "href")[1]
    hrefShort <- unlist(stri_extract_all_regex(href, "[0-9]+"))
    hrefBig <- paste("http://www.opensubtitles.org", href, sep = "")

    name <- "downloaded_subtitles"
    dir <- file.path(getwd(), name)
    if (!file.exists(dir))
        dir.create(file.path(getwd(), name))
    nameWithDir <- paste(name, "/", hrefShort, ".zip", sep = "")
    download.file(hrefBig, nameWithDir, mode = "wb")
    if (unzip) {
        con <- unzip(nameWithDir, exdir = file.path(getwd(), name))
        file.remove(nameWithDir)
        if (!info) {
            file.remove(con[2])
        }
    }
    return("Downloaded.")
}
