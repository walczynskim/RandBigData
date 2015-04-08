#-------------------------------- ONET-----------------------------------------
## The script is downloading articles form webpage:
##"http://wiadomosci.onet.pl/wybory-prezydenckie-2015"

library("rvest")
library("stringi")

OnetArticleInfo <- function(link, id){
  #Function getting basic info about an article from the link
  
   link <- html(link)
   #Article source
   get.from <- "wiadomosci.onet.pl"  
   title <- html_nodes(link, "#mainTitle > h1")[[1]] %>% 
            html_text(encoding="UTF-8") %>% 
            stri_trim()
  
   tags <- html_nodes(link, "#relatedTopics > span")[-1] %>% 
           html_text(encoding="UTF-8") %>%
           stri_trim() %>%
           stri_paste(collapse = ",") %>%
           stri_replace_all_regex(",\\.+", "") 
   
   #Date when the article was published
   date <- html_nodes(link, "#articleHeading > meta") %>% 
           html_attr("content") %>% 
           stri_replace_all_regex("\\+[0-9]{4}", "")
   
   #Articles content
   content1 <- html_nodes(link, "#lead > p") %>% html_text(encoding="UTF-8")
   content <- html_nodes(link,"#detail > p") %>% html_text()
   
   if (length(content) == 0){
      contents <- html_nodes(link, "#detail > div.interview > p") %>% 
                  html_text()
      content <- paste(contents, collapse=" ")  
   } 
   
   content <- stri_paste(content1, content, collapse = " ")

   #Number of comments
   comments.number <- html_nodes(link, "#socialTop > div > div.box3_forum > 
                                        div.forumCommentButton > div.dymek > 
                                        div.dymek4") %>% html_text()
   
   #Getting everything together in data frame
   data.frame("id"=id, "get.from"=get.from, "date"=date, "title"=title, 
              "content"=content, "tags"=tags, "comments.number"=comments.number) 
}

###EXAMPLE
#link <- "http://wiadomosci.onet.pl/kraj/andrzej-duda-w-polsce-
# powinny-powstac-polsko-amerykanskie-bazy-wojskowe/vw98hk"
#OnetArticleInfo(link, 1) 


OnetInfo <- function(link, folder.with.articles, last.time.keeping.file){
   #Function updates the folder.with.articles articles base with the newsets 
   #articles from link 
  
   #We assume that we keep the time of the last update in last.time.keeping.file
   #We don't want to get one article twice
   time <- readLines(last.time.keeping.file) %>% strptime("%Y-%m-%d %H:%M")
   
   #Gettin articles' links
   articles.links <- html(link) %>% 
                     html_nodes("#staticStreamContent > div > a") %>%
                     html_attr("href") %>% unique()
   articles <- data.frame()
   
   #Getting articles' information from links
   for(i in seq_along(articles.links)){
      article <- OnetArticleInfo(articles.links[i], i)
      
      #Checking if the article is new to us
      if (unclass(time - strptime(article$date, "%Y-%m-%d %H:%M")) < 0){
         articles <- rbind(articles, article)
      }
   }
   
   #Updating time file
   id <- Sys.time()
   writeLines(strftime(id,"%Y-%m-%d %H:%M:%S %Z"), 
              last.time.keeping.file) 
   
   #Creating new unique file and putting articles there
   path <- paste(folder.with.articles, "onet", strftime(id, "%Y-%m-%d %H-%M-%S"), 
                 ".txt", collapse="", sep="")
   if (nrow(articles)!=0)
      write.table(articles, path)  
}

source.page <- "http://wiadomosci.onet.pl/wybory-prezydenckie-2015"
folder.with.articles <- "D://"
last.time.keeping.file <- "D://czasonet.txt"
OnetInfo(source.page, folder.with.articles, last.time.keeping.file)
