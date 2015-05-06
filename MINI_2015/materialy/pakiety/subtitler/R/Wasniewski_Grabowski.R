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

pobierz_napisy <- function (tytul, jezyk) {
  if (missing(tytul) | missing(jezyk))
    stop("Nie podales wszystkich argumentow")
  url <- znajdz_tytul_jezyk(tytul, jezyk)
  hrefs_titles <- linki_i_tytuly_na_stronie(url)
  if (length(hrefs_titles) > 0) {
    ktory_tytul <- hrefs_titles["title", ]
    if (!all(duplicated(ktory_tytul)[-1])) {
      print(ktory_tytul)
      cat("Ktorych napisow potrzebujesz?")
      ktory_film <- as.numeric(scan(file = "", what = "",
                                    nmax = 1))
    }
    else ktory_film <- 1
    url_napisow <- paste0("http://opensubtitles.org", hrefs_titles["href",
                                                                   ktory_film])
    pobierz_attrs <- atrybuty_pobierz(url_napisow)
    if (!is.null(pobierz_attrs)) {
      pobierz_rozpakuj(pobierz_attrs)
    }
    else {
      hrefs_titles_zagniezdzone <- linki_i_tytuly_na_stronie(url_napisow)
      if (length(hrefs_titles_zagniezdzone) == 0)
        stop("Niedokladne zapytanie, mozliwe, ze szukasz serialu?")
      zip_url <- paste0("http://opensubtitles.org", hrefs_titles_zagniezdzone["href",
                                                                              1])
      pobierz_attrs2 <- atrybuty_pobierz(zip_url)
      if (!is.null(pobierz_attrs2)) {
        pobierz_rozpakuj(pobierz_attrs2)
      }
      else stop("Nie moge dogrzebac sie do Twoich napisow... ;/")
    }
  }
  else {
    stop("Nie ma napisow dla tego filmu lub w tym jezyku ;(")
  }
}


znajdz_tytul_jezyk <- function(tytul, jezyk){
  if (missing(tytul) | missing(jezyk))
    stop('Nie podano argumentu')
  url <- paste0('http://www.opensubtitles.org/pl/search2/sublanguageid-', jezyk,'/moviename-')
  title <- strsplit(tytul," ")[[1]]
  sciezka <- paste0(url, ifelse(length(title)==1, title, paste0(title,collapse = "+")))
  return(sciezka)
}

linki_i_tytuly_na_stronie <- function(url){
  dostepne_napisy <- html_nodes(html(url),".bnone")
  attrs <- html_attrs(dostepne_napisy)
  hrefs_titles <- sapply(attrs, function(x) x[c("href","title")])
  return(hrefs_titles)
}

atrybuty_pobierz <- function(url_napisow){
  pobierz_button <- html_nodes(html(url_napisow),"#bt-dwl")
  attrs2 <- html_attrs(pobierz_button)
  pobierz <- unlist(attrs2)
  return(pobierz)
}

pobierz_rozpakuj <- function(pobierz_attrs){
  dir_name <- pobierz_attrs["data-installer-file-name"]
  tmp <- tempfile()
  download.file(paste0(pobierz_attrs["rel"],".zip"),tmp)
  if( dir_name%in%dir() )
    message("Uwaga, nadpisanie istniejacych plikow!") else
      dir.create(dir_name)
  unzip(tmp, exdir=dir_name)
  unlink(tmp)
  message("Napisy pobrano, szefie.")
}
