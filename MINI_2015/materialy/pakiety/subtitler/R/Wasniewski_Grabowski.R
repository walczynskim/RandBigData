#' Chmura słów dla napisów filmu
#'
#' Funkcja \code{Wasniewski_Grabowski} zwraca obrazek z chmura słów dla napisow z podanego filmu w danym języku.
#' Obrazek jest zapisywany w working directory w formacie png.
#'
#'
#'
#' @param tytul argument typu character dlugosci 1, tytul filmu (lub jego czesc) dla ktorego ma zostac zrobiony wordcloud
#' @param jezyk argument typu character dlugosci 1, jezyk dla ktorego ma zostac zrobiony wordcloud
#' @param liczbaSlow liczba słóœ jaka ma się znaleść w obrazku
#'
#' @examples
#' Wasniewski_Grabowski('goblet of fire', 'eng', 200)
#' Wasniewski_Grabowski('skyfall', 'eng')
#' @import wordcloud
#' @import stringi
#' @import XML
#' @import rvest
#' @import tm
#'
#' @author Mikołaj Waśniewski, Paweł Grabowski

Wasniewski_Grabowski <- function(tytul, jezyk, liczbaSlow=100) {
  stopifnot(is.character(tytul), is.character(jezyk), length(tytul)==1,
            length(jezyk)==1)
  subtitler::pobierz_napisy(tytul,jezyk)

  tpath <- list.files(pattern=paste0(paste(unlist(strsplit(tytul, " ")),collapse="-"),"-",jezyk))
  tpath2 <- list.files(tpath, pattern = ".txt|.srt")
  napisy <- readLines(paste0(tpath,"/",tpath2))
  napisy2 <- do.call(paste, c(as.list(napisy), sep=" ")) %>%
    enc2utf8(.)

  corpus <- Corpus(VectorSource(napisy2)) %>%
    tm_map(., stripWhitespace) %>%
    tm_map(., content_transformer(tolower)) %>%

    tm_map(., removeWords, stopwords("english")) %>%
    tm_map(., removePunctuation)

  term.matrix <- TermDocumentMatrix(corpus) %>%
    as.matrix(term.matrix)
  png(paste0(tpath2,".png"), width=12, height=8, units="in", res=300)
  commonality.cloud(term.matrix,  scale=c(5,0.5),  max.words=liczbaSlow, random.order=FALSE,
                    rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  dev.off()
  unlink(tpath, recursive = TRUE)
}
