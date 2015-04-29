#' Download subtitles from a given link
#'
#' Function \code{readSubtitle} reads directly from http the subtitle in srt format.
#'
#' @aliases subtitle
#' @param link A link [URL].
#' @param ... Other parameter passed to readLines().
#' @return Character vector with the file's entry.
#' @examples
#' readSubtitle('http://dl.opensubtitles.org/pl/download/file/1954081967')
#' readSubtitleId('1954081967')
#' \dontrun{
#' readSubtitle()
#' }
#' @seealso \code{\link{readLines}} for other arguments.
#' @import ggplot2

readSubtitle <- function(link, ...) {
    readLines(link, ...)
}

readSubtitleId <- function(id, ...) {
  readLines(paste0('http://dl.opensubtitles.org/pl/download/file/',id), ...)
}

