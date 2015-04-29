#' Pobieranie informacji o filmie
#'
#' Funkcja \code{GetMovieInfo} pobiera informacje o filmie z takich serwisow jak:
#' IMDB, Wikipedia, OpenSubtitles
#'
#' @usage GetMovieInfo(title)
#' @param title tytul filmu w jezyku angielskim. Im dokladniejszy tym lepsze
#' jakosciowo informacje o filmie otrzymamy
#'
#' @details
#' Funkcja poprzez podany \code{title} wyszukuje film w serwisie IMDB.
#' Skad pobiera:
#'       tytul polski,
#'       tytul angielski,
#'       gatunki,
#'       rok produkcji,
#'       budzet,
#'       opis,
#'       ocene,
#'       kraje produkcji,
#'       rezyserow,
#'       scenarzystow,
#'       aktorow,
#'       producentow.
#'Nastepnie stara sie znalezc ten film na angielskiej wikipedii skad pobiera
#'analogiczne dane.
#'Na koniec funkcja wyszukuje film w serwisie OpenSubtitles i pobiera link
#'do sciagniecia napisow.
#'
#'@return
#'Wszystkie pobrane informacje sa zapisywane w pliku tekstowym o nazwie takiej
#'jak argument \code{title} w folderze roboczym.
#'
#'@author Karolina Wyszynska
#'
#'@import
#'  stringi
#' rvest
#' XML
#' subtitler
#'
#'@examples
#' GetMovieInfo("Inception")
#' GetMovieInfo("Lord of the rings")
#' GetMovieInfo("Harry Potter")
#'
#'
#'

GetMovieInfo <- function(title){
  stopifnot(is.character(title), length(title)==1)

  #IMDB
  tmp <- stri_replace_all_regex(title, " ", "%20")
  page <- NULL
  try(page <- html(paste0("http://www.imdb.com/find?q=",
                            tmp, "&s=tt&ttype=ft&ref_=fn_ft")), silent=TRUE)
  if (is.null(page)){
    print("Can't find such title on IMDB")
    IMDB <- NULL
  } else {
      nodes <- html_node(page, ".result_text a")
      if(is.null(nodes)){
        print("Can't find such title on IMDB")
        IMDB <- NULL
      } else {
          id <- html_attr(nodes, "href")[1] %>%
                stri_match_first_regex("[0-9]{7}") %>%
                unlist()
          link <- paste0("http://www.imdb.com/title/tt", id, "/")
          page <- html(link)
          #Get info from IMDB

          ##Polish and english title
          nodes <- html_nodes(page, ".header .itemprop")
          polishTitle <- html_text(nodes)
          nodes <- html_nodes(page,".title-extra")
          englishTitle <- html_text(nodes)
          if (length(englishTitle) == 0){englishTitle <- polishTitle}

          ##Kind
          nodes <- html_nodes(page, ".infobar .itemprop")
          kinds <- html_text(nodes)
          if (length(kinds) == 0){ kinds <- ""}

          ##Production year
          nodes <- html_nodes(page, ".header a")
          yearOfProduction <- html_text(nodes)
          if (length(yearOfProduction) == 0){ yearOfProduction <- 0}

          ##Budget
          nodes <- html_nodes(page, "#titleDetails.article>div.txt-block")
          budget <- html_text(nodes)
          budget <- budget[which(stri_detect_regex(budget,"Budget"))]
          if (length(budget) == 0){ budzet <- 0
          } else {
            budget <- stri_replace_all_regex(budget,",","")
            options(scipen = 999)
            budget <- as.numeric(unlist(stri_extract_all_regex(budget, "[0-9]*[0-9]")))
          }

          ##Description
          nodes <- html_nodes(page, "#titleStoryLine p")
          description <- html_text(nodes)
          if (length(description) == 0){
            description <- ""
          } else {
            description <- stri_replace_all_regex(description,"\n|\b|\t|\r|\a","")
          }

          ##Mark
          nodes <- html_nodes(page,"strong span")
          mark <- html_text(nodes)
          if(length(mark) == 0){ mark <- ""}

          ##Countries of production
          nodes <- html_nodes(page,"#titleDetails.article>div.txt-block")
          countries <- html_text(nodes)
          if (length(countries) == 0){countries <- ""} else {
            countries<-countries[stri_detect_regex(countries,"Country:")]
            countries<-stri_replace_all_regex(countries,"\n|\b|\t|\r|\a","")
            countries<-stri_replace_all_regex(countries,"Country:","")
            countries<-unlist(strsplit(countries,"\\|"))
            countries<-unlist(stri_extract_all_regex(countries,"[^ ].*[^ ]"))
          }

          ##Directors, scenarists and actors
          page <- NULL
          try(page <- html(paste0(link, "fullcredits")), silent=TRUE)
          if(is.null(page)){
            directors <- NULL;
            scenarists <- NULL }else{
            nodes <- html_nodes(page, ".simpleCreditsTable:nth-child(2) a")
            directors <- stri_replace_all_regex(html_text(nodes), "\n|\b|\t|\r|\a", "")


            nodes <- html_nodes(page, ".simpleCreditsTable:nth-child(4) a")
            scenarists <- stri_replace_all_regex(html_text(nodes), "\n|\b|\t|\r|\a", "")

            nodes <- html_nodes(page, ".itemprop")
            actors <- stri_replace_all_regex(html_text(nodes), "\n|\b|\t|\r|\a", "") %>%
              stri_trim() %>% unique()
            }

          ##Producers
          page <- html(paste0(link, "companycredits"))
          nodes <- html_nodes(page, "#production+ .simpleList a")
          producers <- html_text(nodes)
          if(length(producers) == 0){producers <- ""}
          producers <- stri_replace_all_regex(producers, '\"', "")


          IMDB <- paste("PolishTitle:\n", polishTitle,
                       "\n\nEnglishTitle:\n", englishTitle,
                       "\n\nKind:\n", paste(kinds, collapse=", "),
                       "\n\nYearOfProduction\n", yearOfProduction,
                       "\n\nBudget\n", budget,
                       "\n\n\nDescription\n", description,
                       "\n\n\nMark\n", mark,
                       "\n\nCountriesOfProduction\n", paste(countries, collapse=", "),
                       "\n\nDirectors\n" , paste(directors, collapse=", \n"),
                       "\n\nScenarists\n", paste(scenarists, collapse=", \n"),
                       "\n\nActors\n", paste(actors, collapse=", \n"),
                       "\n\nProducers\n", paste(producers,collapse=", \n"),
                       sep=" ", collpase=" ")

      }
  }

  #Wiki
  tmp <- stri_replace_all_regex(title, " ", "_")
  page <- NULL
  try(page <- html(paste0("http://en.wikipedia.org/wiki/", tmp)), silent=TRUE)
  if(is.null(page)){
    print("Problem with finding the title on Wikipedia")
    Wiki <- NULL
    } else{
      tables <- NULL
      try(tables <- readHTMLTable(paste0("http://en.wikipedia.org/wiki/", tmp)), silent=TRUE)

       if (is.null(tables)){
        print("Movie page on Wikipedia wasn't found.")
        Wiki <- NULL
       } else{
            if(!is.null(tables) & (!is.matrix(tables[[1]]) & !is.data.frame(tables[[1]]))){
                print("Movie page isn't correctly build.")
                Wiki <- NULL
            } else {
              details <- paste(as.character(tables[[1]][ ,1]),
                               as.character(tables[[1]][ ,2]), sep="  ",
                               collapse="\n")
              nodes <- html_nodes(page, ".vevent+ p")
              description <- html_text(nodes)
              Wiki <- paste("Wikipedia Details:\n\n", details,
                            "\n\n\nWikipedia Description:\n", description)
            }
      }
    }

  #OpenSubtitles
  subtitleLink <- NULL
  try({page <- html("http://www.opensubtitles.org/pl/search2/sublanguageid-eng/moviename-ring+two")
       nodes <- html_node(page, ".bnone")
       link <- html_attr(nodes, "href")
       page <- html(paste0("http://www.opensubtitles.org",link))
       nodes <- html_node(page, ".bnone")
       link <- html_attr(nodes, "href")
       page <- html(paste0("http://www.opensubtitles.org",link))
       nodes <- html_nodes(page, "#bt-dwl")
       subtitleLink <- html_attr(nodes,"rel")}, silent=TRUE)

  #Paste information together
  text <- NULL
  if(is.null(IMDB)){tekst <- Wiki} else {tekst<- IMDB}
  if(!is.null(Wiki)){ tekst <- paste(tekst, Wiki, sep="\n\n\n")}
  if(!is.null(subtitleLink)){ tekst <- paste(tekst,
                                       "\n\nDownload english subtitles at:\n",
                                       subtitleLink, sep="\n")}

  old <- getwd()
  on.exit(setwd(old))

  name <- paste0(title, ".txt")
  file.create(name)
  if(!is.null(tekst)){
    writeLines(tekst, name)
  }

  print("Mission completed")
  invisible(0)
}


