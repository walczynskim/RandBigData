# Sciagnij internety


# 
# source("R/Marta.R")
# source("R/Marcin.R")
library(RCurl)
library(XML)
library(dplyr)
library(stringi)

get_google_page_urls <- function(u) {
   # read in page contents
   html <- getURL(u)
   
   # parse HTML into tree structure
   doc <- htmlParse(html)
   
   # extract url nodes using XPath. Originally I had used "//a[@href][@class='l']" until the google code change.
   links <- xpathApply(doc, "//h3//a[@href]", function(x) xmlAttrs(x)[[1]])
   
   # free doc from memory
   free(doc)
   
   # ensure urls start with "http" to avoid google references to the search page
   links <- grep("http://", links, fixed = TRUE, value=TRUE)
   return(links)
}

daj_mi_linki_zebym_byl_zadowolony <- function( x ){
   sapply(x, function(y){
      paste0( "http://www.google.pl/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=",y ) %>%
         get_google_page_urls() %>% grep( pattern = "/url", value = TRUE) %>% strsplit( "?q=") %>%
         lapply( function(element){ strsplit( element[2], ".pl" )[[1]][1] } ) %>%
         unlist() %>% paste0(".pl") %>% unique()
   }
   )
}


daj_mi_linki_TERAZ_I_JUZ <- function( x ){
   
   daj_mi_linki_zebym_byl_zadowolony( x ) %>%
      unlist( ) %>% grep( pattern = "http://", fixed= TRUE, value = TRUE )
}
library("stringi")
library("rvest")

superfunkcja <- function(main_page_link, dictionary, how_many){
   
   main_page_link <- html(main_page_link)
   link_and_title <- html_nodes(main_page_link, "a")
   links <- html_attr(link_and_title, name="href")
   titles <- html_text(link_and_title)
   
   lista <- lapply(lapply(stri_extract_all_words(titles), stri_trans_tolower), function(x){
      sum((x %in% dictionary) >= how_many)
   })
   
   t <- titles[which(unlist(lista)==1)]
   #g <- guess_encoding(t)$confidence[1]
   #if(g>0.5) repair_encoding(t)
   l <- unlist(links[which(unlist(lista)==1)])
   #gg <- guess_encoding(l)$confidence[1]
   #if(gg>0.5) repair_encoding(l)
   ll <- unlist(stri_extract_all_regex(l, "[h][t][t][p].+"))
   if(length(ll)==0){
      "kicha"
   } else {
      ll <- na.omit(ll)
      tt <- t[l %in% ll]
      data.frame(tytul=tt, link=ll)
   }
}

########################################################################
########################################################################
########################################################################
########################################################################


dictionaryX <- read.table("D:/web-scraping/slownik.txt",encoding = "UTF-8")

                  
tytulo_linki <- list( )
linki_do_main_page <- daj_mi_linki_TERAZ_I_JUZ( c("wiadomosci", "newsy", "swiat", "gazeta") )

for( i in seq_along(linki_do_main_page)  ){
   
   tytulo_linki[[i]] <- superfunkcja( linki_do_main_page[[i]],  dictionary = dictionaryX[,1], how_many = 1 )
   
}
  
lin <- unlist(lapply(tytulo_linki, function(element){
   if(class(element)=="data.frame"){
      as.vector(element$link)
   }
}))

tyt <- unlist(lapply(tytulo_linki, function(element){
   if(class(element)=="data.frame"){
      as.vector(element$tytul)
   }
}))


# dir.create("dane/")
# dir.create("dane/artykuly/")
dir <- "D:/web-scraping/dane/artykuly/"


for( i in seq_along(tyt) ){
   
   element <- lin[i]
   fileName <- paste0(unlist(stri_extract_all_regex(Sys.time(), pattern="[0-9]+")), collapse="")
   file.create( file = paste0( dir, fileName, ".txt" ) )
   write.table(x = element, file = paste0( dir, fileName, ".txt" ), fileEncoding = "UTF-8" )
   write.table(x = tryCatch( repair_encoding(tyt[i]), error = function(cond) tyt[i] ), file = paste0( dir, fileName, ".txt" ), append = TRUE, fileEncoding = "UTF-8" )
   
   text <- html( element ) %>% 
      html_nodes( ".art-lead, .art-content, p, #intertext1,
           .lead,#artykul, #gazeta_article_lead, newsContent, hyphenate" ) %>% 
      html_text() %>% 
      repair_encoding() 
   
   tryCatch( repair_encoding(text), error = function(cond) text ) %>%
   write.table( file = paste0( dir, fileName, ".txt" ), append = TRUE, fileEncoding = "UTF-8" ) 
} 


