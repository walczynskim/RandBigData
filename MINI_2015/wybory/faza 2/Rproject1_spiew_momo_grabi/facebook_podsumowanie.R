#############################################################################
### funkcja: likes_per_day()
### opis:
### funkcja zwraca tabelke danych dla danego kandydata, kt?rej elementami 
### jest liczba 'lajk?w' odnotowana danego dnia
#############################################################################

dir <- "C:\\Users\\MARTYNKA\\Documents\\Wybory\\Facebook"

likes_per_day <- function(){
  
  dir <- paste(dir, "Likes", sep="\\")
  
  kandydant_likes <- list.files(path = dir, pattern="_likes")
  for(name in kandydant_likes){
    
    table <- read.table(paste(dir, name, sep="/"), sep=";", head = TRUE) 
    
    kan <- tbl_df(table)
    kan <- kan[,-4]
    
    likes_per_day <- spread(kan, date, likes)
    write.table(likes_per_day, 
                file = paste(dir, "Podsumowanie", paste0("all_", name), sep="/"), 
                row.names = FALSE, append = FALSE)
  }
}

######################################################################################
### funkcja facebook_sentiment_podsumowanie()
### opis:
### okreslamy wyzdziek komentarzy pod postami umieszczonymi na stronach na temat kandydatow
### dla kazdego postu zliczamy liczbe komentarzy neutralnych, pozytywnych i negatywnych
### je?li suma wynosi -1,0,1 uznajemy komentarza za netralny, je?li suma >=2 to 
### komentarz pozytywny, za? suma <= -2 komentarz negatywny
######################################################################################

source("oczyszczanie.R")
source("sentyment.R")

facebook_sentiment_podsumowanie <- function(){
  require(dplyr)
  
  kandydant_comments <- list.files(path = dir, pattern="_comments")

  for(x in kandydant_comments){
    comments <- read.table(paste(dir, x, sep="\\"), sep=";", head = TRUE)
    # na wszelki wypadek sprawdzamy, czy nie ma powtarzajacych sie komentarzy
    comments <- unique(comments, by = comments.id)
    
    comments_msg <- comments$comments.message
    # oczyszcanie
    comments_msg <- oczyszczanie(comments_msg)
    # sprawdzanie sentymentu
    sentiment_msg <- sentyment(comments_msg)
    
    sentiment <- character(length(sentiment_msg))
    # okre?lamy czy komentarz ma wydzwiek:
    # neutralny - 0, pozytywny - P, negatywny - N
    sentiment[sentiment_msg %in% c(-1,0,1)] <- "0"
    sentiment[sentiment_msg > 1] <- "P"
    sentiment[sentiment_msg < -1] <- "N"
    
    comments %>%
      mutate(sentiment = sentiment) -> comments
    # zapisywanie z dodana kolumna 
    write.table(x = comments, file = paste(dir, "Podsumowanie\\Sentiment", x, sep="\\"), append = FALSE)
    
    # zliaczenie komentarzy 0/N/P dla kazdego postu
    podsumowanie <- lapply(unique(comments$post.id), function(x){
      comments %>% 
        filter(comments$post.id == x) %>%
        summarise(neutral = sum(sentiment == "0"), 
                  possitive = sum(sentiment == "P"),
                  negative = sum(sentiment == "N"),
                  comments_like_counts = mean(post.likes.count))
    })
    
    # dla kazdej strony dodajemy kolemne z zliczona liczba komentarzy o danym wydzwieku
    table_pods <- unique(comments[!duplicated(comments$post.id),c(1:4,7:9)])
    table_pods <- cbind(table_pods, do.call(rbind.data.frame, podsumowanie))
    table_pods <- arrange(table_pods, post.from_name)
    
    # zapisujemy do pliku 
    write.table(x = table_pods, file = paste(dir, "Podsumowanie\\Sentiment", paste0("sentiment_", x), 
                                             sep="\\"), row.names = FALSE)
    
  }
}

####################################################################################
### funkcja facebook_sentiment_podsumowanie()
### opis:
### dla kazdego kandydata dokonujemy podsumowania w podanym okresie od - do, 
### tzn. wyliczamy srednia liczbe polubien postow umieszczonych na stronie, 
### komentarzy pod postami oraz udostepnien postow
####################################################################################

facebook_posts_podsumowanie <- function(od=as.Date("2015-01-01"), do=Sys.Date()){
  require(dplyr)
  require(stringi)
  
  kandydant <- list.files(path = dir, pattern="facebook.csv")
  for(x in kandydant){
    posts <- read.table(paste(dir, x, sep="\\"), sep=";", head = TRUE)
    data <- as.Date(unlist(stri_extract_all_regex(posts$created_time, 
                                                  "[0-9]{4}-[0-9]{2}-[0-9]{2}")))
    # ograniczamy się tylko do podanego odcinka czasowego
    posts %>% 
      filter(data >= od, data <=do) -> posts
    
    tmp_posts <- lapply(unique(posts$from_id), function(y){
      posts %>% 
        filter(from_id == y) %>%
        summarise(likes_count = round(mean(likes_count), 0),
                  comments_count = round(mean(comments_count), 0),
                  shares_count = round(mean(shares_count), 0))
    }) %>%
      do.call(rbind.data.frame, .)
    
    tmp_posts_save <- unique(posts[!duplicated(posts$from_id),c(1:2)])
    tmp_posts_save <- cbind(tmp_posts_save, tmp_posts)
    tmp_posts_save <- arrange(tmp_posts_save, from_name)
    
    # dane zapisujemy do pliku wyjsciowego
    write.table(x = tmp_posts_save, file = paste(dir, "Podsumowanie\\Mean_posts", paste0("mean_", x), 
                                                 sep="\\"), row.names = FALSE)
  }
}
