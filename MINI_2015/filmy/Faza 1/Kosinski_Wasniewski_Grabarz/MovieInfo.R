MovieInfo<-function(url){
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
library(rvest)
library(stringi)
library(dplyr)
# 
# # examples link:
# url<-"http://www.imdb.com/title/tt1074638/"
# url<-"http://www.imdb.com/title/tt0095016/"
# url <-"http://www.imdb.com/title/tt0065908/"
# url<-"http://www.imdb.com/title/tt0460989/"
# url<-"http://www.imdb.com/title/tt0079336/"
# url<-"http://www.imdb.com/title/tt0944947/"
# url <- "http://www.imdb.com/title/tt1666801/"
# 
# MovieInfo(url)
