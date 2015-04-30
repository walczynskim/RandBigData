#' Funkcja pobiera informacje z IMDB, WIKIPEDII i linki do polskich napisow z OPENSUBTITLER.
#' 
#' @param url - bezposredni link do filmu ze strony imdb.com
#' @return lista parametrow z kazdej strony
#' @examples 
#' imdb_wikipedia_subtitles_info(url = "http://www.imdb.com/title/tt0095016/")
#'
#' @import rvest
#' @import stringi
#' @import XML
#' @import dplyr
#' 
#' @author Pawel grabowski
#' 



imdb_wikipedia_subtitles_info <- function(url) {
  if (!is.character(url)) {
    stop("zly url")
  }
  imdbInfo <- MovieInfo(url)
  wikiInfo <- MovieInfo2(url)
  opensubtitlesInfo <- MovieInfo3(url)
  c(imdbInfo, wikiInfo, opensubtitlesInfo)
}

MovieInfo <- function(url) {
  url <- stri_extract_first_regex(url, "http://www.imdb.com/title/[a-z]+[0-9]+/")
  html <- html(url)
  
  title <- html_nodes(html, ".header .itemprop") %>% 
    html_text() %>% 
    ifelse(length(.)>0, ., NA)
  
  
  original_title <- html_nodes(html, ".title-extra") %>%
    html_text() %>%
    stri_extract_all_regex("(?<=\").*(?=\")") %>%
    unlist %>%
    ifelse(length(.)>0, ., title)
  
  genre <- html_nodes(html, ".infobar .itemprop") %>% html_text() %>% 
    stri_paste(collapse=", ") %>% 
    ifelse(length(.)>0, ., NA)
  
  rating <- html_nodes(html, ".star-box-giga-star") %>% 
    html_text()  %>%
    stri_trim() %>% 
    ifelse(length(.)>0, ., NA)
  
  runtime <- html_nodes(html,"#overview-top time") %>% 
    html_text() %>% 
    stri_extract_first_regex("\\d+") %>%
    as.numeric() %>% 
    ifelse(length(.)>0, ., NA)
  
  year <- html_nodes(html, ".header .nobr") %>% 
    html_text() %>%
    stri_extract_first_regex("\\d{4}") %>% 
    as.numeric() %>%
    ifelse(length(.)>0, ., NA)
  
  
  x <- html_nodes(html,"#titleDetails .txt-block") %>% 
    html_text() 
  
  country <- x[stri_detect_fixed(x, "Country:")] %>%
    stri_replace_all_regex("\n|(See more)|»|Country:|[|]", "") %>% 
    stri_extract_all_words() %>%
    unlist() %>%
    stri_paste(collapse=", ") %>%
    ifelse(length(.)>0, ., NA)
  
  language <- x[stri_detect_fixed(x, "Language:")] %>%
    stri_replace_all_regex("\n|(See more)|»|Language:", "") %>%
    stri_extract_all_words() %>%
    unlist() %>%
    stri_paste(collapse=", ") %>%
    ifelse(length(.)>0, ., NA)
  
  budget <- x[stri_detect_fixed(x, "Budget:")] %>%
    stri_replace_all_regex(",", "") %>%
    stri_extract_all_regex("\\d+") %>%
    unlist() %>%
    as.numeric() %>%
    ifelse(length(.)>0, ., NA)
  
  release_date <- x[stri_detect_fixed(x, "Release Date:")] %>%
    stri_replace_all_regex(".*:|\n|(See more)|»", "") %>%
    stri_extract_first_regex(".*(?= [(])") %>%
    stri_trim() %>% 
    ifelse(length(.)>0, ., NA)
  
  opening_weekend <- x[stri_detect_fixed(x, "Opening Weekend:")] %>%
    stri_replace_all_regex(",", "") %>%
    stri_extract_first_regex("\\d+") %>%
    as.numeric() %>% 
    ifelse(length(.)>0, ., NA)
  
  gross <- x[stri_detect_fixed(x, "Gross:")] %>%
    stri_replace_all_regex(",", "") %>%
    stri_extract_first_regex("\\d+") %>%
    as.numeric() %>% 
    ifelse(length(.)>0, ., NA)
  
  production_co <- x[stri_detect_fixed(x, "Production Co")] %>%
    stri_replace_all_regex(".*:|,|(See more)|»|[\"]", "") %>%
    stri_extract_all_regex("(?<=\n).*(?=\n)") %>%
    unlist() %>%
    stri_trim() %>%
    '['(stri_length(.)>0) %>%
    stri_paste(collapse=", ") %>% 
    ifelse(length(.)>0, ., NA)
  
  sound_mix <- x[stri_detect_fixed(x, "Sound Mix:")] %>%
    stri_replace_all_regex(".*:|(See more)|»|[|]|[\"]|[;]", "") %>%
    stri_extract_all_regex("(?<= {4}).*(?=\n)") %>%
    unlist() %>%
    stri_trim() %>%
    '['(stri_length(.)>0) %>%
    stri_paste(collapse=", ") %>% 
    ifelse(length(.)>0, ., NA)
  
  color <- x[stri_detect_fixed(x, "Color:")] %>%
    stri_replace_all_regex(".*:|\n|(See more)|»|[\"]", "") %>%
    stri_trim() %>% 
    ifelse(length(.)>0, ., NA)
  
  aspect_ratio <- x[stri_detect_fixed(x, "Aspect Ratio:")] %>%
    stri_extract_all_regex("(?<=Aspect Ratio: ).*(?=[\n])", "") %>%
    as.vector() %>% 
    ifelse(length(.)>0, ., NA)
  
  html2 <- stri_paste(url,"fullcredits") %>% html() 
  
  director <- html_nodes(html2,".simpleCreditsTable:nth-child(2) a") %>% 
    html_text() %>% stri_trim() %>% 
    ifelse(length(.)>0, .,  NA)
  
  writers <- html_nodes(html2,".simpleCreditsTable:nth-child(4) a") %>% 
    html_text() %>% stri_trim() %>% stri_paste(collapse = ", ") %>% 
    ifelse(length(.)>0, .,  NA)
  
  cast <- html_nodes(html2,".itemprop") %>% html_text() %>%
    stri_trim() %>% unique() %>% stri_paste(collapse = ", ") %>% 
    ifelse(length(.)>0, .,  NA)
  
  keywords <- stri_paste(url, "keywords") %>% html() %>% 
    html_nodes(".sodatext a") %>% html_text() %>% 
    stri_paste( collapse=", ") %>% ifelse(length(.)>0, .,  NA)
  
  list(title=title, original_title=original_title, genre=genre, rating=rating,
       runtime = runtime, year=year, country=country, 
       language=language, release_date=release_date, 
       budget=budget, gross=gross, opening_weekend=opening_weekend,
       production_co=production_co, color=color,
       aspect_ratio=aspect_ratio, sound_mix=sound_mix, director = director,
       writers=writers, cast=cast, keywords=keywords
  )
  
}  

MovieInfo2 <- function(url) {
  url <- stri_extract_first_regex(url, "http://www.imdb.com/title/[a-z]+[0-9]+/") %>%
    stri_paste(., "externalsites", sep = "")
  
  htmlSite <- html(url)
  
  tempMisc <- html_nodes(htmlSite, ".simpleList:nth-child(7)") %>%
    html_text
  
  
  wikiLinksPositions <-  html_nodes(htmlSite, ".simpleList:nth-child(7) a") %>%
    html_text() %>%
    stri_detect_fixed(., "Wikipedia")
  
  linktoWikipedia <- html_nodes(htmlSite, ".simpleList:nth-child(7) a")[wikiLinksPositions] %>%
    html_attr(., "href") %>%
    stri_paste("http://www.imdb.com", ., sep = "" )
  
  if (length(linktoWikipedia) > 0) {
    tempDf <- readHTMLTable(linktoWikipedia, stringsAsfactor = FALSE)[[1]] %>%
      na.omit()
    #row.names(tempDf) <- NULL
    tempList <- lapply(as.character(tempDf[[2]]), FUN = function(x) x)
    names(tempList) <- as.character(tempDf[[1]])
    return(tempList)
  } else {
    res <- list(wikipediaData = "no wikipedia data found")
    return(res)
  }
  
}


MovieInfo3 <- function(url) {
  movieIdImdb <- stri_extract_first_regex(url, "(?<=http://www.imdb.com/title/)[a-z]+[0-9]+")
  urlTemp <- stri_paste("http://www.opensubtitles.org/pl/search/sublanguageid-pol/imdbid-",
                        movieIdImdb, sep = "")
  htmlSite <- html(urlTemp)
  moviesNames <-  html_nodes(htmlSite, ".bnone") %>%
    html_text() %>%
    stri_replace_all_regex(.,"\n \t\t\t|;", "")
  moviesDownloadLinks <- html_nodes(htmlSite, "#search_results td:nth-child(5) a") %>%
    html_attr("href") %>%
    stri_paste("http://www.opensubtitles.org",.)
  dfTemp <- data.frame(Nazwa = moviesNames, Link = moviesDownloadLinks)
  return(list(linkiDoNapisow = dfTemp))
}
