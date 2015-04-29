#--------------------------------GAZETA PL--------------------------------------
## The script is downloading articles form webpage:
##"http://wiadomosci.gazeta.pl/wiadomosci/0,114916.html?tag=
##wybory+prezydenckie+2015"

library(rvest)
library(stringi)

GazetaPlArtykulInfo <- function(link,id){
  #Function getting basic info about an article from the link
  
   link <- html(link)
   #Date when the article was published
   date <- html_nodes(link, "#gazeta_article_date") %>% html_text()
   dates <- stri_extract_all_regex(date,"[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}") %>%
           unlist() %>% stri_replace_all_regex("\\.", "-")
   times <- unlist(stri_extract_all_regex(date,"[0-9]{2}:[0-9]{2}"))
   date <- paste(dates, times)

   title <- html_nodes(link,"h1") %>% html_text(encoding="UTF-8")

   get.from <- "gazeta.pl"
   
   content <- html_nodes(link, "#artykul , #gazeta_article_lead") %>%
              html_text(encoding="UTF-8") %>% stri_paste(collapse=" ")

   tags <- html_nodes(link, "#gazeta_article_tags ul") %>% 
           html_text(encoding="UTF-8")
   if (length(tags) == 0)
     tags=""
  
   #Number of comments
   comments.number <- html_nodes(link, ".head a") %>% html_text() 
   if (length(comments.number) == 0)
     comments.number <- html_nodes(link, ".head") %>% html_text()
   
   comments.number <- as.numeric(stri_extract_all_regex(comments.number, 
                                                        "[0-9]{1,10}"))
   if(is.na(comments.number))
     comments.number=0
   
   #Getting everything together in data frame
   data.frame("id"=id, "get.from"=get.from, "date"=date, "title"=title, 
              "content"=content, "tags"=tags, "comments.number"=comments.number) 
} 

###EXAMPLE
#link <- "http://wiadomosci.gazeta.pl/wiadomosci/1,114871,17575348,
#         Nowy_sondaz_CBOS_u__Dwucyfrowy_spadek_notowan_Komorowskiego.html"
#GazetaPlArtykulInfo(link, 1)


GazetaPlInfo <- function(link, folder.with.articles, last.time.keeping.file){
   #Function updates the folder.with.articles articles base  with the newsets 
   #articles from link 
  
   #We assume that we keep the time of the last update in last.time.keeping.file
   #We don't want to get one article twice
   time <- readLines(last.time.keeping.file) %>% strptime("%d-%m-%Y %H:%M")
   
   #Getting links to this day's news
   link <- html(link)
   news.links <- html_nodes(link,"h2 a") %>% html_attr("href")
   news <- html_nodes(link, ".base") %>% html_text(encoding="UTF-8") 
   daynews <- stri_extract_all_regex(news, "Wiadomoœci dnia") %>% unlist()
   daynews <- which(!is.na(daynews))
   news.links <- news.links[daynews]

   articles <- data.frame()

   for(i in seq_along(news.links)){
      article <- GazetaPlArtykulInfo(news.links[i], i)
      #Checking if the article is new to us
      if (unclass(time - strptime(article$date, "%d-%m-%Y %H:%M")) < 0)
         articles <- rbind(articles, article)   
   }
   
   #Updating time file
   id <- Sys.time()
   writeLines(strftime(id,"%d-%m-%Y %H:%M"), 
              last.time.keeping.file) 
   
   #Creating new unique file and putting articles there
   path <- paste(folder.with.articles, "/gazetapl", 
                 strftime(id, "%Y-%m-%d %H-%M-%S"), ".txt", collapse="", sep="")
   if (nrow(articles)!=0)
     write.table(articles, path)  
}
   
source.page<-"http://wiadomosci.gazeta.pl/wiadomosci/0,114916.html?tag=
              wybory+prezydenckie+2015"
folder.with.articles <- "D://pop2"
last.time.keeping.file <- "D://pop2/czasgazetapl.txt"
GazetaPlInfo(source.page, folder.with.articles, last.time.keeping.file)






