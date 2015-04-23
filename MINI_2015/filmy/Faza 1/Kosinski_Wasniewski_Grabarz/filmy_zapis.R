
urlDf <- read.table("C:\\Users\\grabarze\\Documents\\movies2\\MoviesUrl.txt")




GetMovieInfo <- function(x) {
  temp <- MovieInfo(x) %>% 
    paste0(., collapse='";"', sep="") %>%
    paste0('"', ., '"')
    
  f <- file("MoviesData.txt", open="a")
  writeLines(temp, f)
  close(f)
}


lapply(urlDf[10001:11000,], GetMovieInfo)


#xx <- read.table("MoviesData.txt", sep=";", header=TRUE)
  
