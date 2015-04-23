 
# http://www.imdb.com/search/title

url <- "http://www.imdb.com/search/title?count=250&release_date=,2015-04&title_type=feature&view=simple"


urlList <- vector(mode = "list", 1000)
urlList[1] <- url
iter <- 0

i=1
while (i <= 100) {
  iter <- iter + 1
  print(iter)
  # links to movie page
  linksMovies <- html(url) %>% html_nodes(".title > a") %>% html_attr("href") %>% 
     stri_paste("http://www.imdb.com", .)
  
  
  

   f <- file("MovieData.txt", open="a")

  close(f)
#   
  oneMovie <- vector(mode = "list", 250)
  oneMovie <- lapply(linksMovies[1:2], MovieInfo)
  perSiteMovies <- do.call(rbind.data.frame, oneMovie)
  #df <- as.data.frame(df)
  write.table(perSiteMovies, "MoviesData.txt",
              sep=";",
              append = TRUE,
              col.names = FALSE,
              row.names = FALSE)
  

  #d <- read.table("film_test.txt", sep=";")
  
  
  # next page:
  url <- html(url) %>% 
    html_nodes("#right a") %>%
    html_attr("href") %>%
    '['(length(.)) %>%
    stri_paste("http://www.imdb.com", .)
  urlList[iter+1] <- url
  print(urlList[[iter+1]])
  
  i <- i + 1

}




# sciaganie linkow
url <- "http://www.imdb.com/search/title?count=250&release_date=,2015-04&title_type=feature&view=simple"
iter <- 0
fname <- "MoviesUrl.txt"

i=1
while(i <= 1000) {
  iter <- iter + 1
  print(iter)
  linksMovies <- html(url) %>% html_nodes(".title > a") %>% html_attr("href") %>% 
    stri_paste("http://www.imdb.com", .)
  
  f <- file(fname, open = 'a')
  lapply(linksMovies, FUN = writeLines, f)
  close(f)
  
  url <- html(url) %>% 
    html_nodes("#right a") %>%
    html_attr("href") %>%
    '['(length(.)) %>%
    stri_paste("http://www.imdb.com", .)
  i=i+1
}
