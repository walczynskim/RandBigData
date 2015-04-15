
library(rvest)
library(XML)
library(stringi)


ParseGazetaPl <- function(){
   
   # Date of publication:
   data <- as.character(Sys.Date())
   
   # Url of currently analysed site
   url <- "http://www.gazeta.pl"
   
   # Data frame containing all info about articles from specific HTML page:
   df <- data.frame()
   
   # Parsing an HTML page:
   parsed_page <- htmlParse(url)#,encoding="UTF-8")
   
   ###############################################
   ### Info about main articles;   weight = 4 ###
   main_articles <- getNodeSet(parsed_page, "//div[@class='col-md-8 col-sm-8 col-xs-12 mt_pict']//a")
   
   # Titles of main articles:
   if(length(main_articles)>0){
         
      main_titles <- sapply(main_articles, xmlValue, "href")
      main_titles <- main_titles[1]
      main_titles <- stri_replace_all_regex(main_titles, ';', "")
      main_titles <- stri_trim_both(stri_replace_all_regex(main_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
      
      # Links to articles
      url_main_titles <- sapply(main_articles, xmlGetAttr, "href")
      url_main_titles <- url_main_titles[1]
      
      
      ktore <- sapply(main_titles, function(x) sprawdz_czy_wybory(slownik,x))   
      
      if(sum(ktore)>0){
         
         df<-rbind(df, data.frame(date=data, portal=url, title=main_titles[ktore], position=4,link=url_main_titles[ktore])) 
      }
   }
   ###############################################
   ##Infor about main2 articles; weight = 3 ###
   
   main2_articles <- getNodeSet(parsed_page, "//div[@class='row mod_photo_box mod_section type_foreheads']//a")
   
   # Titles of main2 articles:
   main2_titles <- sapply(main2_articles,html_attr, "title")
   main2_titles  <- unique(main2_titles)
   main2_titles <- stri_replace_all_regex(main2_titles, ';', "")
   main2_titles <- stri_trim_both(stri_replace_all_regex(main2_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   # Links to articles
   url_main2_titles <- sapply(main2_articles, html_attr, "href")
   url_main2_titles <- unique(url_main2_titles)
   url_main2_titles <- unlist(sapply(url_main2_titles, function(x){ if(stri_extract_first_regex(x,".{4}")!="http"){ paste("http://",x, sep="")}else x}))
   names(url_main2_titles) <- NULL
   
   ktore <- sapply(main2_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df<-rbind(df, data.frame(date=data, portal=url, title=main2_titles[ktore], position=3,link=url_main2_titles[ktore])) 
   }
   
   ###############################################
   ##Infor about right articles; weight = 2 ### 
   
   right_articles <- html_nodes(parsed_page, ".box_news .news_box_pict a")
   
   # Titles of right articles:
   right_titles <- sapply(right_articles,html_attr,"title")
   right_titles  <- unique(right_titles)
   right_titles <- stri_replace_all_regex(right_titles, ';', "")
   right_titles <- stri_trim_both(stri_replace_all_regex(right_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   
   # Links to articles
   url_right_titles <- sapply(right_articles, html_attr, "href")
   url_right_titles <- unique(url_right_titles)
   url_right_titles <- unlist(sapply(url_right_titles, function(x){ if(stri_extract_first_regex(x,".{4}")!="http"){ paste("http://",x, sep="")}else x}))
   names(url_right_titles) <- NULL
   
   ktore <- sapply(right_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df<-rbind(df, data.frame(date=data, portal=url, title=right_titles[ktore], position=2,link=url_right_titles[ktore])) 
   }
   
   
   ##Infor about other aticles, weight = 1 ###
   
   other_articles <- html_nodes(parsed_page, ".box_news .news_box_links a")
   
   
   # Titles of other articles:
   other_titles <- sapply(other_articles,html_attr,"title")
   other_titles  <- unique(other_titles)
   other_titles <- stri_replace_all_regex(other_titles, ';', "")
   other_titles <- stri_trim_both(stri_replace_all_regex(other_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   # Links to articles
   url_other_titles <- sapply(other_articles, html_attr, "href")
   url_other_titles <- unique(url_other_titles)
   url_other_titles <- unlist(sapply(url_other_titles, function(x){ if(stri_extract_first_regex(x,".{4}")!="http"){ paste("http://",x, sep="")}else x}))
   names(url_main2_titles) <- NULL
   
   ktore <- sapply(other_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df<-rbind(df, data.frame(date=data, portal=url, title=other_titles[ktore], position=1,link=url_other_titles[ktore])) 
   }
   
   
   if(dim(df)[1]>0){
      
      fname<-paste(getwd(),"/",data,".csv", sep="")
      
      if (!file.exists(fname)){
         f <-file(fname, open="a")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                               '\"link\"', sep = ";"), f)
      }else f <- file(fname, open="a")
      
      for(i in seq_along(df[,1])){
         #dopisuje do pliku kolejny wiersz      
         writeLines(stri_paste(df[i,1], df[i,2], df[i,3],
                               df[i,4], df[i,5], sep=";"),f)
      }
      close(f)
      
   }
   return(invisible(NULL))
   # #artykul
   
}

ParseDziennikPl <- function(){

   # Date of publication:
   data <- as.character(Sys.Date())
   
   # Url of currently analysed site
   url <- "http://www.dziennik.pl"
   
   # Data frame containing all info about articles from specific HTML page:
   df <- data.frame()
   
   # Parsing an HTML page:
   parsed_page <- html(url)
   
   
   ###############################################
   ### Info about main articles;   weight = 4 ###
   main_articles <- html_nodes(parsed_page, ".box-big-news-right a")
   if(length(main_articles)>0){
   # Titles of main articles:
      main_titles <- sapply(main_articles,xmlValue,"href")
      main_titles <- main_titles[main_titles!=""]
      main_titles <- stri_replace_all_regex(main_titles, ';', "")
      main_titles <- stri_trim_both(stri_replace_all_regex(main_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
      
      
      # Links to articles
      url_main_titles <- sapply(main_articles, xmlGetAttr, "href")
      url_main_titles <- url_main_titles[1]
      
      ktore <- sapply(main_titles, function(x) sprawdz_czy_wybory(slownik,x))   
      
      if(sum(ktore)>0){
         
         df <- rbind(df, data.frame(date=data, portal=url, title=main_titles[ktore], position=4,link=url_main_titles[ktore])) 
      }
   
   }
   ###############################################
   ##Infor about main2  articles; weight = 3 ###
   main2_articles <- html_nodes(parsed_page, ".top h2 a")
   
   # Titles of main articles:
   main2_titles <- sapply(main2_articles,html_attr, "title")
   main2_titles <- stri_replace_all_regex(main2_titles, ';', "")
   main2_titles <- stri_trim_both(stri_replace_all_regex(main2_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   # Links to articles
   url_main2_titles <- sapply(main2_articles, html_attr, "href")
   
   ktore <- sapply(main2_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df  <- rbind(df, data.frame(date=data, portal=url, title=main2_titles[ktore], position=3,link=url_main2_titles[ktore])) 
   }
   
   ###############################################
   ##Infor about right articles; weight = 2 ###
   
   right_articles <- html_nodes(parsed_page, ".topn h4 a")
   
   # Titles of main articles:
   right_titles <- sapply(right_articles,html_attr, "title")
   right_titles <- stri_replace_all_regex(right_titles, ';', "")
   right_titles <- stri_trim_both(stri_replace_all_regex(right_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   # Links to articles
   url_right_titles <- sapply(right_articles, html_attr, "href")
   
   ktore <- sapply(right_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df <- rbind(df, data.frame(date=data, portal=url, title=right_titles[ktore], position=2,link=url_right_titles[ktore])) 
   }
   
   ###############################################
   ##Infor about other articles; weight = 1 ###
   
   other_articles <- html_nodes(parsed_page, "#box_inside_sg .dbl_left .font_red")
   
   # Titles of other articles:
   other_titles <- sapply(other_articles,html_attr, "title")
   other_titles <- stri_replace_all_regex(other_titles, ';', "")
   other_titles <- stri_trim_both(stri_replace_all_regex(other_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   # Links to articles
   url_other_titles <- sapply(other_articles, html_attr, "href")
   
   ktore <- sapply(other_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df <- rbind(df, data.frame(date=data, portal=url, title=other_titles[ktore], position=1,link=url_other_titles[ktore])) 
   }
   
   if(dim(df)[1]>0){   
      fname<-paste(getwd(),"/",data,".csv", sep="")
      
      if (!file.exists(fname)){
         f <-file(fname, open="a")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                               '\"link\"', sep = ";"), f)
      }else f <- file(fname, open="a")
      
      
      for(i in seq_along(df[,1])){
         #dopisuje do pliku kolejny wiersz      
         writeLines(stri_paste(df[i,1], df[i,2], df[i,3],
                               df[i,4], df[i,5], sep=";"),f)
      }
      close(f)
      
   }
   
   return(invisible(NULL))
   
   # intertext1 p
   
   
}




ParseWyborczaPl <- function(){
   
   # Date of publication:
   data <- as.character(Sys.Date())
   
   # Url of currently analysed site
   url <- "http://wyborcza.pl/0,0.html?piano_d=1"
   
   # Data frame containing all info about articles from specific HTML page:
   df <- data.frame()
   
   # Parsing an HTML page:
   parsed_page <- html(url)
   
   
   ###############################################
   ### Info about main articles;   weight = 4 ###
   main_articles <- html_nodes(parsed_page, "#LinkArea\\:MagMagSwi1 ")
   #.cnt2 #LinkArea\\:MT a
   # Titles of main articles:
   if(length(main_articles)>0){
      main_titles <- sapply(main_articles, xmlGetAttr, "title")   
      main_titles <- unlist(main_titles[!sapply(main_titles, is.null)])
      main_titles <- stri_replace_all_regex(main_titles, ';', "")
      main_titles <- stri_trim_both(stri_replace_all_regex(main_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
      
      # Links to articles
      url_main_titles <- sapply(main_articles, xmlGetAttr, "href")
      url_main_titles <- unlist(url_main_titles[!sapply(url_main_titles, is.null)])
      
      ktore <- sapply(main_titles, function(x) sprawdz_czy_wybory(slownik,x))   
      
      if(sum(ktore)>0){
         
         df<-rbind(df, data.frame(date=data, portal=url, title=main_titles[ktore], position=4,link=url_main_titles[ktore])) 
      }
   }
   ###############################################
   ##Infor about main2 articles; weight = 3 ###
   
   main2_articles <- html_nodes(parsed_page, ".article #LinkArea\\:kraj")
   
   # Titles of main articles:
   main2_titles <- xmlToDataFrame(main2_articles)
   main2_titles <- as.vector(main2_titles$text)
   main2_titles <- stri_replace_all_regex(main2_titles, ';', "")
   main2_titles <- stri_trim_both(stri_replace_all_regex(main2_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   main2_titles <- main2_titles[main2_titles!=""]
   # Links to articles
   url_main2_titles <- sapply(main2_articles, html_attr, "href")
   url_main2_titles <- unique(url_main2_titles)   

   ktore <- sapply(main2_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df<-rbind(df, data.frame(date=data, portal=url, title=main2_titles[ktore], position=3,link=url_main2_titles[ktore])) 
   }
   
   ###############################################
   ##Infor about right articles; weight = 2 ###
   
   right_articles <- html_nodes(parsed_page, ".article #LinkArea\\:najnowsze")
   
   # Titles of right articles:
   right_titles  <- xmlToDataFrame(right_articles)
   right_titles <- as.vector(right_titles$text)
   right_titles <- right_titles[sapply(right_titles,function(x) x!="\n")]
   right_titles <- stri_replace_all_regex(right_titles, ';', "")
   right_titles <- stri_trim_both(stri_replace_all_regex(right_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   # Links to articles
   url_right_titles <- sapply(right_articles, html_attr, "href")
   url_right_titles <- unique(url_right_titles)
   
   ktore <- sapply(right_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df<-rbind(df, data.frame(date=data, portal=url, title=right_titles[ktore], position=2,link=url_right_titles[ktore])) 
   }
   
   ###############################################
   ##Infor about other articles; weight = 1 ###
   
   other_articles <- html_nodes(parsed_page, "#bottom_wrap .content a")
   
   # Titles of other articles:
   other_titles <- sapply(other_articles,html_attr, "title")
   other_titles <- stri_replace_all_regex(other_titles, ';', "")
   other_titles <- stri_trim_both(stri_replace_all_regex(other_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   # Links to articles
   url_other_titles <- sapply(other_articles, html_attr, "href")
   
   ktore <- sapply(other_titles, function(x) sprawdz_czy_wybory(slownik,x))   
   
   if(sum(ktore)>0){
      
      df<-rbind(df, data.frame(date=data, portal=url, title=other_titles[ktore], position=3,link=url_other_titles[ktore])) 
   }
   
   if(dim(df)[1]>0){
      
      
      fname<-paste(getwd(),"/",data,".csv", sep="")
      
      if (!file.exists(fname)){
         f <-file(fname, open="a")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                               '\"link\"', sep = ";"), f)
      }else f <- file(fname, open="a")
      
      for(i in seq_along(df[,1])){
         #dopisuje do pliku kolejny wiersz      
         writeLines(stri_paste(df[i,1], df[i,2], df[i,3],
                               df[i,4], df[i,5], sep=";"),f)
      }
      close(f)
      
   }
   
   
   return(invisible(NULL))
   
   # artykul
   
   
}


ParseOnetPl <- function(){
   onet <- data.frame()
   data <- as.character(Sys.Date())
   url_onet <- "http://www.onet.pl/"
   doc <- html(url_onet)
   #GLOWNY - 4
   glowna_text <- html_text(html_nodes(doc,".titleContener .title"))
   glowna_text <- stri_replace_all_regex(glowna_text,"(\\n)|(\\r)|(\\t)|(;)"," ")
   glowna <- getNodeSet(doc, "//div[@id='bestOfOnet']//a") 
   glowna_link <- xmlGetAttr(glowna[[1]],"href")
   
   
   ktore<-sapply(glowna_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      onet<-rbind(onet, data.frame(date=data, portal=url_onet,
                                   title=glowna_text[ktore], position=4,link=glowna_link[ktore])) 
   }
   
   #   onet<-rbind(onet, data.frame(date=data, portal=url_onet,
   #                                   title=glowna_text, position=4,link=glowna_link))
   #   
   #PODGLOWNY - 3
   podglowna_text<-html_text(html_nodes(doc,".bottomItem .title"))
   podglowna_text<-stri_replace_all_regex(podglowna_text,"(\\n)|(\\r)|(\\t)|(;)"," ")
   podglowna<-getNodeSet(doc, "//div[@class='bestOfOnetSliderFrameInnerStatic']//a")
   podglowna_link<-sapply(podglowna, xmlGetAttr, "href")
   
   ktore<-sapply(podglowna_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      onet<-rbind(onet, data.frame(date=data, portal=url_onet,
                                   title=podglowna_text[ktore], position=3,link=podglowna_link[ktore])) 
   }
   
   #   
   #Z BOKU - 2
   bok_text <- html_text(html_nodes(doc,"#boxNews .title"))
   bok_text <- stri_replace_all_regex(bok_text,"(\\n)|(\\r)|(\\t)|(;)"," ")
   bok <- getNodeSet(doc, "//div[@class='boxContent']//a")
   bok <- bok[1:length(bok_text)]
   bok_link <- sapply(bok, xmlGetAttr, "href")
   
   ktore <- sapply(bok_text,function(y) sprawdz_czy_wybory(slownik,y))
   if(sum(ktore)>0){
      onet<-rbind(onet, data.frame(date=data, portal=url_onet,
                                   title=bok_text[ktore], position=2,
                                   link=bok_link[ktore])) 
   }
   
   #DOL - 1   
   dol_text <- html_text(html_nodes(doc,"#boxLocalNews .title , #boxEconomy .title"))
   dol_text <- stri_replace_all_regex(dol_text,"(\\n)|(\\r)|(\\t)|(;)"," ")
   
   linki1 <- sapply(getNodeSet(doc, "//section[@data-section='economy']/div[@class='boxContent']//a"),xmlGetAttr,"href")
   linki2 <- sapply(getNodeSet(doc, "//section[@data-section='localnews']/div[@class='boxContent']//a"),xmlGetAttr,"href")
   linki3 <- sapply(getNodeSet(doc, "//section[@data-section='localnews2']/div[@class='boxContent']//a"),xmlGetAttr,"href")
   dol_link <- c(linki1,linki2,linki3)
   
   ktore <- sapply(dol_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   
   if(sum(ktore)>0){
      
      onet<-rbind(onet, data.frame(date=data, portal=url_onet,
                                   title=dol_text[ktore], position=1,link=dol_link[ktore])) 
   }
   
   
   ### ZAPISYWANIE
   
   if(dim(onet)[1]>0){
      
      fname<-paste0(getwd(),"\\",data, ".csv")
      
      if (!file.exists(fname)){
         f<-file(fname, open="a",encoding="UTF-8")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                               '\"link\"', sep = ";"), f)
      }
      else f<-file(fname, open="a",encoding="UTF-8")
      n<-nrow(onet)
      for(i in 1:n){
         #dopisuje do pliku kolejny wiersz      
         writeLines(stri_paste(onet[i,1], onet[i,2], onet[i,3],
                               onet[i,4], onet[i,5], sep=";"),f)
      }
      close(f)
   }
   
   return(invisible(NULL))
}


ParseTvn24Pl <- function(){

   url<-"http://www.tvn24.pl/"
   doc<-html(url)
   
   tvn<-data.frame()
   data<-as.character(Sys.Date())
   
   glowna_text<-stri_trim_both(stri_replace_all_regex(html_text(html_nodes(doc,".mainContainer :nth-child(1) :nth-child(1) h1 a")),"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))[1:2]
   glowna <- html_node(doc, "#urgentStandard h1 a")
   #getNodeSet(doc, "//div[@class='Article-List-Whole hyphenate special_3items block spacing10 ']//a")
   #urgentStandard h1
   #html_node(doc, "#urgentStandard h1 a")
   #glowna_link<-sapply(glowna,html_attr,"href")
   glowna_link <- html_attr(glowna,"href")
   glowna_link<-unname(sapply(glowna_link,function(y){
      
      if(stri_detect_fixed(y,"http")==FALSE){
         
         return(stri_flatten(c("http://www.tvn24.pl",y)))
      } else return(y)
   }))
   
   ktore<-sapply(glowna_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      tvn<-rbind(tvn, data.frame(date=data, portal=url,
                                 title=glowna_text[ktore], position=4,link=glowna_link[ktore])) 
   }
   
   #PODGLOWNA - 3
   podglowna<-getNodeSet(doc, "//div[@class='mainLeftColumn']//a")
   podglowna_text<-sapply(podglowna,xml_text,"title")
   niepotrzebne<-stri_detect_regex(podglowna_text,"(zobacz wi?cej)|(czytaj dalej)|(^[0-9][0-9]?$)|(?i)(Odtw?rz)")
   podglowna_text<-podglowna_text[!niepotrzebne]
   podglowna_link<-sapply(podglowna,xmlGetAttr,"href")
   podglowna_link<-podglowna_link[!niepotrzebne]
   niepotrzebne2<-which(podglowna_text=="")
   podglowna_text<-stri_trim_both(stri_replace_all_regex(podglowna_text[-niepotrzebne2],"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
   podglowna2_text<-podglowna_text[1:8]
   podglowna_link<-podglowna_link[-niepotrzebne2]
   podglowna2_link<-podglowna_link[1:8]
   podglowna2_link<-unname(sapply(podglowna2_link,function(y){
      
      if(stri_detect_fixed(y,"http")==FALSE){
         
         return(stri_flatten(c("http://www.tvn24.pl",y)))
      } else return(y)
   }))
   
   
   ktore<-sapply(podglowna_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      tvn<-rbind(tvn, data.frame(date=data, portal=url,
                                 title=podglowna_text[ktore], position=3,link=podglowna_link[ktore])) 
   }
   
   #BOK - 2
   bok<-getNodeSet(doc, "//ul[@id='newestNewsList']//a")
   bok_text<-stri_trim_both(stri_replace_all_regex(sapply(bok,xml_text,"title"),"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
   bok_link<-sapply(bok,xmlGetAttr,"href")
   bok_link<-unname(sapply(bok_link,function(y){
      
      if(stri_detect_fixed(y,"http")==FALSE){
         
         return(stri_flatten(c("http://www.tvn24.pl",y)))
      } else return(y)
   }))
   
   ktore<-sapply(bok_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      tvn<-rbind(tvn, data.frame(date=data, portal=url,
                                 title=bok_text[ktore], position=2,link=bok_link[ktore])) 
   }
   
   #TEZ BOK - 2
   
   bok<-getNodeSet(doc, "//ul[@id='importantNewsList']//a")
   bok_text<-stri_trim_both(stri_replace_all_regex(sapply(bok,xml_text,"title"),"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
   bok_link<-sapply(bok,xmlGetAttr,"href")
   bok_link<-unname(sapply(bok_link,function(y){
      
      if(stri_detect_fixed(y,"http")==FALSE){
         
         return(stri_flatten(c("http://www.tvn24.pl",y)))
      } else return(y)
   }))
   
   ktore<-sapply(bok_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      tvn<-rbind(tvn, data.frame(date=data, portal=url,
                                 title=bok_text[ktore], position=2,link=bok_link[ktore])) 
   }
   
   # DOL - 1
   dol<-podglowna
   dol_text<-podglowna_text[-(1:8)]
   dol_link<-podglowna_link[-(1:8)]
   dol_link<-unname(sapply(dol_link,function(y){
      
      if(stri_detect_fixed(y,"http")==FALSE){
         
         return(stri_flatten(c("http://www.tvn24.pl",y)))
      } else return(y)
   }))
   
   ktore<-sapply(dol_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      tvn<-rbind(tvn, data.frame(date=data, portal=url,
                                 title=dol_text[ktore], position=1,link=dol_link[ktore])) 
   }
   
   # do pliku
   if(dim(tvn)[1] > 0){
      
      fname <- paste0(getwd(), "\\",data, ".csv")
      if (!file.exists(fname)) {
         f <- file(fname, open="a", encoding="UTF-8")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                               '\"link\"', sep = ";"), f)
      }
      else f <- file(fname, open="a", encoding="UTF-8")
      # liczba wierszy w wp_df
      n <- nrow(tvn)
      for(i in 1:n){
         #dopisuje do pliku kolejny wiersz      
         writeLines(stri_paste(tvn[i,1], tvn[i,2], tvn[i,3],
                               tvn[i,4], tvn[i,5], sep=";"),f)
      }
      close(f)
   }
   
   return(invisible(NULL))
   
}

ParseNatematPl <- function() {
   url <- "http://natemat.pl/"
   doc <- html(url)
   natemat  <-data.frame()
   data <- as.character(Sys.Date())
   #GLOWNA - 4
   glowna <- getNodeSet(doc, "//div[@id='hp-body']//a")
   glowna_text <- stri_trim_both(stri_replace_all_regex(xml_text(glowna,"alt"),
                                                        "(\\n)|(\\t)|(\\r)|(\")|(;)"," "))[2]
   glowna_link <- sapply(glowna,xmlGetAttr,"href")[2]
   
   ktore <- sapply(glowna_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
         natemat <- rbind(natemat, data.frame(date=data, portal=url,
                                         title=glowna_text[ktore],
                                         position=4,link=glowna_link[ktore])) 
   }
   
   #PODGLOWNA - 3
   podglowna <- glowna
   podglowna_text <- stri_trim_both(stri_replace_all_regex(xml_text(
      podglowna,"alt"),"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
   niepotrzebne <- which(podglowna_text=="")
   podglowna_text <- podglowna_text[-niepotrzebne]
   niepotrzebne2 <- stri_detect_regex(podglowna_text,"Wiecej zdjec \\([0-9]")
   podglowna_text <- podglowna_text[!niepotrzebne2]
   podglowna_text <- podglowna_text[2:7]
   podglowna_link <- sapply(podglowna,xmlGetAttr,"href")
   podglowna_link <- podglowna_link[-niepotrzebne]
   podglowna_link <- podglowna_link[!niepotrzebne2]
   podglowna_link <-  podglowna_link[2:7]
   
   ktore <- sapply(podglowna_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      natemat<-rbind(natemat, data.frame(date=data, portal=url,
                                         title=podglowna_text[ktore],
                                         position=3,link=podglowna_link[ktore]))
   }
   
   #BOK - 2
   bok <- podglowna
   bok_text <- stri_trim_both(stri_replace_all_regex(xml_text(
      bok,"alt"),"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
   niepotrzebne <- which(bok_text=="")
   bok_text<-bok_text[-niepotrzebne]
   niepotrzebne2<-stri_detect_regex(bok_text,"Wiecej zdjec \\([0-9]")
   bok_text<-bok_text[!niepotrzebne2]
   bok_text<-bok_text[9:16]
   bok_link<-sapply(bok,xmlGetAttr,"href")
   bok_link<-bok_link[-niepotrzebne]
   bok_link<-bok_link[!niepotrzebne2]
   bok_link<-bok_link[9:16]
   
   ktore<-sapply(bok_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      natemat<-rbind(natemat, data.frame(date=data, portal=url,
                                         title=bok_text[ktore], position=2,link=bok_link[ktore]))
   }
   
   #DOL - 1
   dol<-podglowna
   dol_text<-stri_trim_both(stri_replace_all_regex(xml_text(dol,"alt"),"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
   niepotrzebne<-which(dol_text==""|is.na(dol_text))
   dol_text<-dol_text[-niepotrzebne]
   niepotrzebne2<-stri_detect_regex(dol_text,"Wiecej zdjec \\([0-9]")
   dol_text<-dol_text[!niepotrzebne2]
   dol_text<-dol_text[-(1:16)]
   dol_link<-sapply(dol,xmlGetAttr,"href")
   dol_link<-dol_link[-niepotrzebne]
   dol_link<-dol_link[!niepotrzebne2]
   dol_link<-dol_link[-(1:16)]
   
   ktore<-sapply(dol_text,function(y) sprawdz_czy_wybory(slownik,y))
   
   if(sum(ktore)>0){
      
      natemat<-rbind(natemat, data.frame(date=data, portal=url,
                                         title=dol_text[ktore],
                                         position=1,link=dol_link[ktore]))
   }
   
   if(dim(natemat)[1]>0){
      
      fname<-paste0(getwd(),"\\",data, ".csv")
      if (!file.exists(fname)){
         f<-file(fname, open="a",encoding="UTF-8")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                               '\"link\"', sep = ";"), f)
      }
      else f<-file(fname, open="a",encoding="UTF-8")
      # liczba wierszy w wp_df
      n<-nrow(natemat)
      for(i in 1:n){
         #dopisuje do pliku kolejny wiersz      
         writeLines(stri_paste(natemat[i,1], natemat[i,2], natemat[i,3],
                               natemat[i,4], natemat[i,5], sep=";"),f)
      }
      close(f)
   }
   
   return(invisible(NULL))
   
}


ParseWpPl <- function(){

   wp_df <- data.frame()
   data <- as.character(Sys.Date())
   url_wp <- "http://www.wp.pl"
   
   doc <- html(url_wp)
   
   ### GLOWNA - 4
   glowna <- getNodeSet(doc, "//div[@id='Container']//a")
   urls_glowna <- sapply(glowna, xmlGetAttr, "href")
   tytuly_glowna<-sapply(glowna, xmlValue, "href")
   tytuly_glowna<-stri_replace_all_regex(tytuly_glowna, ';', "")
   tytuly_glowna<-stri_trim_both(stri_replace_all_regex(tytuly_glowna,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   spr<-which(sapply(tytuly_glowna, function(x) sprawdz_czy_wybory(slowa_klucz, x)))
   #urls_glowna <- urls_glowna[spr]
   #tytuly_glowna <- tytuly_glowna[spr]
   
   if(length(spr)!=0){
      wp_df<-rbind(wp_df, data.frame(date=data, portal=url_wp, 
                                     title=tytuly_glowna[spr], position=4, link= urls_glowna[spr]))
   }
   
   
   ### PODG?OWNA - 3
   
   podglowna <- getNodeSet(doc, "//div[@class='sArt ']//a")
   urls_podglowna <- sapply(podglowna, xmlGetAttr, "href")
   tytuly_podglowna<-sapply(podglowna, xmlValue, "href")
   tytuly_podglowna<-stri_replace_all_regex(tytuly_podglowna, ';', "")
   tytuly_podglowna<-stri_trim_both(stri_replace_all_regex(tytuly_podglowna,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   
   spr<-which(sapply(tytuly_podglowna, function(x) sprawdz_czy_wybory(slowa_klucz, x)))
   if(length(spr)!=0){
      wp_df<-rbind(wp_df, data.frame(date=data, portal=url_wp, 
                                     title=tytuly_podglowna[spr], position=3, link= urls_podglowna[spr]))
   }
   
   ### PRAWY - 2
   
   prawy <- getNodeSet(doc, '//div[@class="txtArts"]//a')
   urls_prawy <- sapply(prawy, xmlGetAttr, "href")
   tytuly_prawy <- sapply(prawy, xmlValue, "href")
   tytuly_prawy <- stri_replace_all_regex(tytuly_prawy, ';', "")
   tytuly_prawy <- stri_trim_both(stri_replace_all_regex(tytuly_prawy,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   
   spr<-as.vector(which(sapply(tytuly_prawy, function(x) sprawdz_czy_wybory(slowa_klucz, x))))
   urls_prawy<-urls_prawy[spr]
   tytuly_prawy<-tytuly_prawy[spr]
   
   if(length(spr)!=0){
      wp_df<-rbind(wp_df, data.frame(date=data, portal=url_wp, 
                                     title=tytuly_prawy, position=2, link=urls_prawy))
   }
   
   ### INNE - 1
   # wiadomosci #wyboryprezydenckie
   url_wp_wp<-"http://wiadomosci.wp.pl/kat,140394,title,Wybory-prezydenckie-w-2015-r,raport.html"
   
   doc_wp<-html(url_wp_wp)
   wybory<-html_nodes(doc_wp, ".stampZrodloData , .kontener h2 a")
   urls_wybory<-na.omit(html_attr(wybory, "href"))
   tmp<-html_text(wybory)
   tmp<-stri_replace_all_regex(tmp, ';', "")
   tmp<-stri_trim_both(stri_replace_all_regex(tmp,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   tytuly_wybory<-tmp[seq(1, length(tmp), by=2)]
   data_wybory<-tmp[seq(2, length(tmp), by=2)]
   ok<-which(data_wybory==data)
   
   if(length(ok)!=0)
      wp_df<-rbind(wp_df, data.frame(date=data_wybory[ok], portal=url_wp, 
                                     title=tytuly_wybory[ok], position=1, link=urls_wybory[ok]))
   # nie sprawdzamy, bo sa o wyborach, # na wp.pl
   
   ### ZAPISYWANIE
   # liczba wierszy w wp_df
   n<-nrow(wp_df)
   if(n!=0){
      fname<-paste0(getwd(),"/", data, ".csv")
      
      if (file.exists(fname)) f<-file(fname, open="a", encoding="")
      if (!file.exists(fname)){
         f<-file(fname, open="a", encoding="")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"',
                               '\"position\"', '\"link\"', sep = ";"), f)
      }
      for(i in 1:n){
         #dopisuje do pliku kolejny wiersz      
         writeLines(stri_paste(wp_df[i,1],wp_df[i,2],wp_df[i,3],wp_df[i,4],wp_df[i,5], 
                               sep=";"),f)
         #          writeLines(stri_paste(paste0('"', wp_df[i,1],'"'), 
         #                                paste0('"', wp_df[i,2],'"'), 
         #                                paste0('"', wp_df[i,3],'"'),
         #                                paste0('"', wp_df[i,4],'"'), 
         #                                paste0('"', wp_df[i,5],'"'), sep=","),f)
      }
      close(f)
   }
}



ParseWprostPl <- function(){
   df <- data.frame()
   data <- as.character(Sys.Date())
   url <- "http://www.wprost.pl/"
   doc <- html(url)
   ### GLOWNA - 4
   glowna <- getNodeSet(doc, "//h2[@class='art-title']//a")
   urls_glowna <- sapply(glowna, xmlGetAttr, "href")
   tytuly_glowna <- sapply(glowna, xmlValue, "href")
   tytuly_glowna <- stri_replace_all_regex(tytuly_glowna, ';', "")
   tytuly_glowna <- stri_trim_both(stri_replace_all_regex(tytuly_glowna,"(\\n)|(\\t)|(\\r)|(\")"," "))
      
   spr <- which(sapply(tytuly_glowna, function(x) sprawdz_czy_wybory(slowa_klucz, x)))
   urls_glowna <- urls_glowna[spr]
   tytuly_glowna <- tytuly_glowna[spr]
   
   if(length(spr) != 0){
      df<-rbind(df, data.frame(date=data, portal=url, 
                               title=tytuly_glowna, position=4, link=urls_glowna))
   }
   
   ### PODG?OWNA - 3
   podglowna <- getNodeSet(doc, "//h3[@class='art-title']//a")
   urls_podglowna <- sapply(podglowna, xmlGetAttr, "href")
   tytuly_podglowna<-sapply(podglowna, xmlValue, "href")
   tytuly_podglowna<-stri_replace_all_regex(tytuly_podglowna, ';', "")
   tytuly_podglowna<-stri_trim_both(stri_replace_all_regex(tytuly_podglowna,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   spr<-which(sapply(tytuly_podglowna, function(x) sprawdz_czy_wybory(slowa_klucz, x)))
   
   if(length(spr)!=0){
      df<-rbind(df, data.frame(date=data, portal=url, 
                               title=tytuly_podglowna[spr], position=3, link= urls_podglowna[spr]))
   }
   ### PRAWY - 2   
   prawy <- getNodeSet(doc, '//ul[@class="newest-list-items"]//a')
   urls_prawy <- sapply(prawy, xmlGetAttr, "href")
   urls_prawy <- paste0("http://www.wprost.pl", urls_prawy)
   tytuly_prawy <- sapply(prawy, xmlValue, "href")
   tytuly_prawy <- stri_replace_all_regex(tytuly_prawy, ';', "")
   tytuly_prawy <- stri_trim_both(stri_replace_all_regex(tytuly_prawy,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   spr <- as.vector(which(sapply(tytuly_prawy, function(x) sprawdz_czy_wybory(slowa_klucz, x))))
   urls_prawy <- urls_prawy[spr]
   tytuly_prawy <- tytuly_prawy[spr]
   if(length(spr)!=0){
      df<-rbind(df, data.frame(date=data, portal=url, 
                               title=tytuly_prawy, position=2, link=urls_prawy))
   }
   
   ### INNE - 1
   # wiadomosci #wyboryprezydenckie
   url1<-"http://www.wprost.pl/tylko-u-nas/"
   doc1<-html(url1)
   
   inne <- getNodeSet(doc1, 
                      '//span[@class="standard-box-title standard-box-offset-300 standard-box-title-large"]//a')
   urls_inne <- sapply(inne, xmlGetAttr, "href")
   urls_inne <- paste0("http://www.wprost.pl", urls_inne)
   tytuly_inne<-sapply(inne, xmlValue, "href")
   tytuly_inne<-stri_replace_all_regex(tytuly_inne, ';', "")
   tytuly_inne<-stri_trim_both(stri_replace_all_regex(tytuly_inne,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   
   spr<-as.vector(which(sapply(tytuly_inne, function(x) sprawdz_czy_wybory(slowa_klucz, x))))
   urls_inne<-urls_inne[spr]
   tytuly_inne<-tytuly_inne[spr]
   
   if(length(spr)!=0){
      df<-rbind(df, data.frame(date=data, portal=url, 
                               title=tytuly_inne, position=1, link=urls_inne))
   }
   
   inne1<- getNodeSet(doc1, '//span[@class="standard-box-title standard-box-offset-175 "]//a')
   urls_inne1 <- sapply(inne1, xmlGetAttr, "href")
   urls_inne1 <- paste0("http://www.wprost.pl", urls_inne1)
   tytuly_inne1<-sapply(inne1, xmlValue, "href")
   tytuly_inne1<-stri_replace_all_regex(tytuly_inne1, ';', "")
   tytuly_inne<-stri_trim_both(stri_replace_all_regex(tytuly_inne,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   
   spr<-as.vector(which(sapply(tytuly_inne1, function(x) sprawdz_czy_wybory(slowa_klucz, x))))
   urls_inne1<-urls_inne1[spr]
   tytuly_inne1<-tytuly_inne1[spr]
   
   if(length(spr)!=0){
      df<-rbind(df, data.frame(date=data, portal=url, 
                               title=tytuly_inne1, position=1, link=urls_inne1))
   }
   
   
   ### ZAPISYWANIE
   n<-nrow(df)
   if(n!=0){
      fname<-paste0(getwd(),"/", data, ".csv")
      
      if (file.exists(fname)) f<-file(fname, open="a")
      if (!file.exists(fname)){
         f<-file(fname, open="a")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', 
                               '\"position\"', '\"link\"', sep = ";"), f)
      }
      
      for(i in 1:n){
         #dopisuje do pliku kolejny wiersz 
         writeLines(stri_paste(df[i,1],df[i,2],df[i,3],df[i,4],df[i,5], 
                               sep=";"),f)
         #          writeLines(stri_paste(paste0('"', df[i,1],'"'), 
         #                                paste0('"', df[i,2],'"'), 
         #                                paste0('"', df[i,3],'"'),
         #                                paste0('"', df[i,4],'"'), 
         #                                paste0('"', df[i,5],'"'), sep=","), f)
      }
      close(f)
   }
}


ParseTvpInfo <- function() {
   
   df<-data.frame()
   data<-as.character(Sys.Date())
   url<-"http://www.tvp.info/"
   
   doc<-html(url)
   
   ### GLOWNA - 4
   glowna <- getNodeSet(doc, "//div[@class='img rel']//a")
   
   urls_glowna <- sapply(glowna, xmlGetAttr, "href")
   urls_glowna <- unique(paste0("http://www.tvp.info.pl", urls_glowna))
   
   tytuly_glowna<-sapply(glowna, xmlValue, "href")
   tytuly_glowna<-stri_replace_all_regex(tytuly_glowna, ';', "")
   tytuly_glowna<-stri_trim_both(stri_replace_all_regex(tytuly_glowna,"(\\n)|(\\t)|(\\r)|(\")"," "))
   tytuly_glowna<-unique(stri_trim_both(tytuly_glowna))[seq(1, length(tytuly_glowna), by=2)]
   
   spr<-which(sapply(tytuly_glowna, function(x) sprawdz_czy_wybory(slowa_klucz, x)))
   urls_glowna <- urls_glowna[spr]
   tytuly_glowna <- tytuly_glowna[spr]
   
   if(length(spr)!=0){
      df<-rbind(df, data.frame(date=data, portal=url, 
                               title=tytuly_glowna, position=4, link=urls_glowna))
   }
   
   ### PODG?OWNA - 3
   
   podglowna <- getNodeSet(doc, "//div[@class='list-info mod-top3 cols3 clr border-bot']//a")
   urls_podglowna <- sapply(podglowna, xmlGetAttr, "href")
   urls_podglowna <- unique(paste0("http://www.tvp.info", urls_podglowna))
   
   tytuly_podglowna <- sapply(podglowna, xmlValue, "href")
   tytuly_podglowna <- stri_replace_all_regex(tytuly_podglowna, ';', "")
   tytuly_podglowna <- stri_trim_both(stri_replace_all_regex(tytuly_podglowna,"(\\n)|(\\t)|(\\r)|(\")"," "))
   tytuly_podglowna <- stri_trim_both(tytuly_podglowna)[seq(1, length(tytuly_podglowna), by=2)]
   
   spr<-which(sapply(tytuly_podglowna, function(x) sprawdz_czy_wybory(slowa_klucz, x)))
   
   if(length(spr)!=0){
      df<-rbind(df, data.frame(date=data, portal=url, 
                               title=tytuly_podglowna[spr], position=3,
                               link= urls_podglowna[spr]))
   }
   
   ### PRAWY - 2
   prawy <- getNodeSet(doc, '//div[@class="list-info mod-top2 cols2 border-bot clr"]//a')
   urls_prawy <- sapply(prawy, xmlGetAttr, "href")
   urls_prawy <- unique(paste0("http://www.tvp.info.pl", urls_prawy))
   
   tytuly_prawy <- sapply(prawy, xmlValue, "href")
   tytuly_prawy <- stri_replace_all_regex(tytuly_prawy, ';', "")
   tytuly_prawy <- stri_trim_both(stri_replace_all_regex(
      tytuly_prawy,"(\\n)|(\\t)|(\\r)|(\")"," "))
   tytuly_prawy <- stri_trim_both(tytuly_prawy)[seq(1, length(tytuly_prawy),
                                                    by=2)]
   
   spr <- as.vector(which(sapply(tytuly_prawy, function(x)
      sprawdz_czy_wybory(slowa_klucz, x))))
   urls_prawy <- urls_prawy[spr]
   tytuly_prawy <- tytuly_prawy[spr]
   
   if(length(spr)!=0){
      df <- rbind(df, data.frame(date=data, portal=url, 
                               title=tytuly_prawy, position=2, link=urls_prawy))
   }
   
   ### WYBORY PREZYDENCKIE W POLSCE
   # wiadomosci #wyboryprezydenckie
   #    url<-"http://www.tvp.info/18103128/wybory-prezydenckie-w-polsce"
   #    doc<-html(url)
   #    wybory<-getNodeSet(doc, "//article[@class='listing']//a")
   #    
   #    
   #    urls_wybory <- sapply(wybory, xmlGetAttr, "href")
   #    urls_wybory <- paste0("http://www.tvp.info", urls_wybory)
   #    urls_wybory<-urls_wybory[!(dup<-duplicated(urls_wybory))]
   #    #urls_wybory <- unique(urls_wybory)
   #    
   #    parse_urls_wybory<-sapply(urls_wybory, html)
   #    
   #    parse_urls_wybory_data<-sapply(parse_urls_wybory, html_nodes, ".data")
   #    parse_urls_wybory_data<-sapply(parse_urls_wybory_data, html_text)
   #    
   #    parse_urls_wybory_data<-sapply(parse_urls_wybory_data, 
   #                                   function(x){
   #                                      tmp<-stri_extract_all_regex(x, "(?<=publikacja: ).*")
   #                                      tmp<-as.Date(strptime(tmp, '%d.%m.%Y'))
   #                                      return(tmp)
   #                                   }
   #    )
   #    
   #    tytuly_wybory<-stri_trim_both(sapply(wybory, xmlValue, "href"))
   #    tytuly_wybory<-stri_replace_all_regex(tytuly_wybory, ';', "")
   #    tytuly_wybory<-stri_trim_both(stri_replace_all_regex(tytuly_wybory,"(\\n)|(\\t)|(\\r)|(\")"," "))
   #    tytuly_wybory<-tytuly_wybory[!dup]
   #    
   #    names(parse_urls_wybory_data)<-NULL
   #    parse_urls_wybory_data <- as.Date(unlist(parse_urls_wybory_data), origin = "1970-01-01")
   #    ok<-which(parse_urls_wybory_data==data)
   #    urls_wybory<-urls_wybory[ok]
   #    tytuly_wybory<-tytuly_wybory[ok]
   #    parse_urls_wybory_data<-parse_urls_wybory_data[ok]
   #    
   #    if(length(ok)!=0)
   #       df<-rbind(df, data.frame(date=data, portal=url, 
   #                                title=tytuly_wybory, position=2, link=urls_wybory))
   #    
   ### ZAPISYWANIE
   n <- nrow(df)
   if(n!=0){
      fname <- paste0(getwd(),"/", data, ".csv")
      
      if (file.exists(fname)) f <- file(fname, open="a",encoding="UTF-8")
      if (!file.exists(fname)){
         f <- file(fname, open="a",encoding="UTF-8")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', 
                               '\"position\"', '\"link\"', sep = ";"), f)
      }
      
      for(i in 1:n) {
         #dopisuje do pliku kolejny wiersz      
         writeLines(stri_paste(df[i,1],df[i,2],df[i,3],df[i,4],df[i,5], 
                               sep=";"),f)
         #          writeLines(stri_paste(paste0('"', df[i,1],'"'), 
         #                                paste0('"', df[i,2],'"'), 
         #                                paste0('"', df[i,3],'"'),
         #                                paste0('"', df[i,4],'"'), 
         #                                paste0('"', df[i,5],'"'), sep=","),f)
      }
      close(f)
   }
}




ParseArchTvn24PL <- function(){
   
   url <- "http://www.tvn24.pl/szukaj.html?q=wybory+prezydenckie&r=2"
   
   data <- as.character(Sys.Date())
   
   doc <- html(url)
   
   tvn<-data.frame()
   
   info <- getNodeSet(doc, "//div[@class='searchResult']//a")
   info_text <- sapply(info,xml_text,"title")
   n <- 33
   
   wygenerowane <- character(n)
   wygenerowane[1] <- url
   
   for(i in 2:n) {
      wygenerowane[i] <- stri_paste(url,"&page=",i)
   }
   
   for(j in 1:n){
      
      doc <- html(wygenerowane[j])
      info <- getNodeSet(doc, "//div[@class='searchResult']//a")
      info_text <- sapply(info,xml_text,"title")
      info_linki <- sapply(info,xmlGetAttr,"href")
      niepotrzebne <- stri_detect_regex(info_text,"(^[0-9][0-9]?$)")
      info_text <- info_text[!niepotrzebne]
      info_linki <- info_linki[!niepotrzebne]
      info_text <- sapply(info_text, function(y) stri_flatten(unlist(stri_extract_all_words(y)),collapse=" "))
      niepotrzebne2 <- which(is.na(info_text)|info_text=="wi?cej"|info_text=="data publikacji"|info_text=="trafno??")
      info_text <- stri_trim_both(stri_replace_all_regex(info_text[-niepotrzebne2],"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
      info_linki <- info_linki[-niepotrzebne2]
      info_linki <- unname(sapply(info_linki,function(z){
         
         if(stri_detect_fixed(z,"http")==FALSE){
            
            return(stri_flatten(c("http://www.tvn24.pl",z)))
         } else return(z)
      }))
      
      info <- getNodeSet(doc, "//time")
      info_daty<-sapply(info,xml_text,"time")
      dni <- unlist(stri_extract_all_regex(info_daty,"(?<= )[0-9]{1,2} "))
      miesiac <- unlist(stri_extract_all_regex(info_daty,"(?<= )[a-z]{3,3}"))
      rok <- unlist(stri_extract_all_regex(info_daty," [0-9]{4,4}"))
      daty <- stri_paste(dni,miesiac,rok)
      daty <- as.character(as.Date(as.POSIXct(daty,format="%d %b %Y")))
      
      ktore <- sapply(info_text,function(z) sprawdz_czy_wybory(slownik,z))
      #     print(daty)
      #     print(daty[ktore])
      #     print(info_text[ktore])
      if(sum(ktore)>0){
         tvn <- rbind(tvn,data.frame(date=daty[ktore],
                                   portal="http://www.tvn24.pl/",
                                   title=info_text[ktore],
                                   position=2,link=info_linki[ktore]))         
      } 
      
   }
   
   # do pliku
   fname <- paste0(getwd(),"\\",data, ".csv")
   if (!file.exists(fname)) {
      f <- file(fname, open="a")
      #tworze pierwszy wiersz w pliku:
      writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"',
                            '\"position\"', '\"link\"', sep = ";"), f)
   }
   else f <- file(fname, open="a")
   # liczba wierszy w wp_df
   n <- nrow(tvn)
   for(i in 1:n){
      #dopisuje do pliku kolejny wiersz      
      writeLines(stri_paste(tvn[i,1], tvn[i,2], tvn[i,3],
                            tvn[i,4], tvn[i,5], sep=";"),f)
   }
   close(f)
   
}


ParseWpolitycePl <- function(n=1) {
   
   wygenerowane <- character(n)
   
   for(i in 1:n){
      wygenerowane[i]<-stri_paste(
         "http://wpolityce.pl/szukaj?q=wybory%20prezydenckie&page=",i)
   }
   
   
   polityka <- data.frame()
   data <- as.character(Sys.Date())
   
   for(i in 1:n) {
      
      url<-wygenerowane[i]
      ##url<-"http://wpolityce.pl/szukaj?q=wybory%20prezydenckie&page=4"
      doc<-html(url)
      
      teksty <- stri_trim_both(stri_replace_all_regex(html_text(
         html_nodes(doc,"h3 a")),"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
      #linki
      linki <- html_attr(html_nodes(doc,"h3 a"),"href")
      
      linki <- unname(sapply(linki,function(y) {         
         if(stri_detect_fixed(y,"http")==FALSE){            
            return(stri_flatten(c("http://www.wpolityce.pl",y)))
         } else {
            return(y)
         }
      }))
      
      #daty
      info <- getNodeSet(doc, "//time")
      info_daty <- sapply(info,xml_text,"time")
      daty <- unlist(stri_extract_all_regex(info_daty,"[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2} "))
      daty <- as.character(as.Date(as.POSIXct(daty,format="%Y-%m-%d")))
      
      ktore<- sapply(teksty,function(y) sprawdz_czy_wybory(slownik,y))
      
      if(sum(ktore)>0){         
         polityka<-rbind(polityka, data.frame(date=daty[ktore], 
                                              portal=url,
                                              title=teksty[ktore], 
                                              position=2,link=linki[ktore])) 
      }
      
      
   }
   
   ### ZAPISYWANIE
   if (nrow(polityka)!=0) {      
      fname<-paste0(getwd(),"\\",data, ".csv")
      if (!file.exists(fname)){
         f<-file(fname, open="a", encoding="UTF-8")
         #tworze pierwszy wiersz w pliku:
         writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                               '\"link\"', sep = ";"), f)
      } else {
         f<-file(fname, open="a", encoding="UTF-8")
      }
      n <- nrow(polityka)
      for(i in 1:n){     
         writeLines(stri_paste(polityka[i,1], polityka[i,2], polityka[i,3],
                               polityka[i,4], polityka[i,5], sep=";"),f)
      }
      close(f)
   
   }
   
   return(invisible(NULL))
   
}

ParseNewsweekPl <- function(){
  library(rvest)
  library(XML)
  library(stringi)
  
  url<-"http://www.newsweek.pl/"
  doc<-html(url)
  
  news<-data.frame()
  data<-as.character(Sys.Date())
  
  #GLOWNA - 4
  glowna_text<-stri_trim_both(stri_replace_all_regex(html_text(html_nodes(doc,"h2"))[1:3],"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
  glowna <- getNodeSet(doc, "//div[@class='Article-List-Whole hyphenate special_3items block spacing10 ']//a")
  glowna_link<-sapply(glowna,xmlGetAttr,"href")
  glowna_text
  
  ktore<-sapply(glowna_text,function(y) sprawdz_czy_wybory(slownik,y))
  
  if(sum(ktore)>0){
    
    news<-rbind(news, data.frame(date=data, portal=url,
                                 title=glowna_text[ktore], position=4,link=glowna_link[ktore])) 
  }
  
  #PODGLOWNA - 3
  podglowna_text<-stri_trim_both(stri_replace_all_regex(html_text(html_nodes(doc,"h2"))[4:8],"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
  podglowna<-getNodeSet(doc, "//div[@class='Article-List-Whole hyphenate block spacing10 ']//a")
  podglowna_link<-sapply(podglowna,xmlGetAttr,"href")
  podglowna_link<-podglowna_link[2:6]
  
  ktore<-sapply(podglowna_text,function(y) sprawdz_czy_wybory(slownik,y))
  
  if(sum(ktore)>0){
    
    news<-rbind(news, data.frame(date=data, portal=url,
                                 title=podglowna_text[ktore], position=3,link=podglowna_link[ktore])) 
  }
  
  #BOK - 2
  bok_text<-stri_trim_both(stri_replace_all_regex(html_text(html_nodes(doc,"h2"))[14:18],"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
  bok<-getNodeSet(doc, "//div[@id='articleList27']//a")
  bok_link<-sapply(bok,xmlGetAttr,"href")[3:7]
  
  ktore<-sapply(bok_text,function(y) sprawdz_czy_wybory(slownik,y))
  
  if(sum(ktore)>0){
    
    news<-rbind(news, data.frame(date=data, portal=url,
                                 title=bok_text[ktore], position=2,link=bok_link[ktore])) 
  }
  
  #DOL - 1
  #articleList1 h2 , #articleList35 h2 , #articleList28 h2
  #dol_text<-stri_trim_both(stri_replace_all_regex(html_text(html_nodes(doc,"#articleList1 h2 , #articleList35 h2 , #articleList28 h2")),"(\\n)|(\\t)|(\\r)|(\")"," "))
  #dol_text
  dol<-getNodeSet(doc, "//div[@class='Article-List-Whole hyphenate block spacing10 ']//a")
  dol_link<-sapply(dol,xmlGetAttr,"href")[16:29][-c(7,12)]
  dol_text<-sapply(dol,xml_text,"title")[16:29]
  dol_text<-stri_trim_both(stri_replace_all_regex(dol_text,"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
  dol_text<-unname(sapply(dol_text,function(y) stri_flatten(unlist(stri_extract_all_words(y)),collapse=",")))[-c(7,12)]
  
  ktore<-sapply(dol_text,function(y) sprawdz_czy_wybory(slownik,y))
  
  if(sum(ktore)>0){
    
    news<-rbind(news, data.frame(date=data, portal=url,
                                 title=dol_text[ktore], position=1,link=dol_link[ktore])) 
  }
  
  dol2<-getNodeSet(doc, "//div[@id='articleList1']//a")
  dol2_link<-sapply(dol2,xmlGetAttr,"href")[-10]
  dol2_text<-sapply(dol2,xml_text,"title")
  dol2_text<-stri_trim_both(stri_replace_all_regex(dol2_text,"(\\n)|(\\t)|(\\r)|(\")|(;)"," "))
  dol2_text<-unname(sapply(dol2_text,function(y) stri_flatten(unlist(stri_extract_all_words(y)),collapse=",")))[-10]
  
  ktore<-sapply(dol_text,function(y) sprawdz_czy_wybory(slownik,y))
  
  if(sum(ktore)>0){
    
    news<-rbind(news, data.frame(date=data, portal=url,
                                 title=dol2_text[ktore], position=1,link=dol2_link[ktore])) 
  }
  
  ### ZAPISYWANIE
  if(dim(news)[1]>0){
    
    fname<-paste0(getwd(),"\\",data, ".csv")
    if (!file.exists(fname)){
      f<-file(fname, open="a",encoding="UTF-8")
      #tworze pierwszy wiersz w pliku:
      writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                            '\"link\"', sep = ";"), f)
    }
    else f<-file(fname, open="a",encoding="UTF-8")
    # liczba wierszy w wp_df
    n<-nrow(news)
    for(i in 1:n){
      #dopisuje do pliku kolejny wiersz      
      writeLines(stri_paste(news[i,1], news[i,2], news[i,3],
                            news[i,4], news[i,5], sep=";"),f)
    }
    close(f)
  }
  return(invisible(NULL))
}



sprawdz_czy_wybory <- function(slownik, tekst){
   
   tekst <- stri_trans_tolower(tekst)
   tmp <- unlist(stri_extract_all_words(tekst))
   res <- any(tmp%in%slownik) | any(tmp%in%stri_trans_tolower(slownik) )
   
   return(res)
   
}



slowa_klucz<-c("bronisław", "bronisława", "bronisławowi", "bronisława", 
               "bronisławem", "bronisławie", "bronisławie", "bronislaw",
               "bronislawa", "bronislawowi","bronislawem","bronkobus",
               "komorowski",
               "komorowskiego", "komorowskiemu", "komorowskiego", 
               "komorowskim", "komorowskim", "komorowski", "bronkobus",
               "bronkobusa","komorowskich","komorowscy",
               "andrzej", "andrzeja", "andrzejowi", "andrzeja", "andrzejem",
               "andrzeju",
               "duda","dudy","dudę","dude","dudą","dudzie",
               "janusz", "janusza", "januszowi", "januszem", "januszu",
               "korwin-mikke", "korwin", "mikke", "jkm",
               "korwin-mikkego","korwina",
               "wybory","wyborach" ,"prezydent","prezydenta", "prezydenckie",
               "kampania","kampanią","kampanii","sondaż","pkw","ogórek","ogorek", "kukiz",
               "jarubas","jarubasa","palikot","palikota","anna grodzka","anny grodzkiej",
               "annę grodzką","annie grodzkiej","marian kowalski","mariana kowalskiego",
               "wanda nowicka","wandy nowickiej","kukiz","kukiza","palikot","palikota",
               "#Braun","Grzegorz Braun","Braun","Brauna",
               "#AndrzejDuda","Andrzej Duda","Dudę","Dudą","Dudzie",
               "#Grodzka","Anna Grodzka","Grodzka","Grodzkiej","Grodzką",
               "#Jankowski","Zdzisław Jankowski","Jankowski",
               "#Jarubas","Adam Jarubas","Jarubas","jarubas", "jarubasa","palikot", "palikota",
               "#Jędrzejewski","Józef Jędrzejewski","Jędrzejewski","Jedrzejewskiego","Jędrzejewskiemu",
               "#KorabKarpowicz","Włodzimierz Korab-Karpowicz","Korab-Karpowicz",
               "#Komorowski","Bronisław Komorowski","Komorowski","Komorowskiego","Komorowskiemu",
               "#Korwin","#KornwinMikke","Janusz Korwin-Mikke", "#JKM","JKM","KORWIN","#KORWIN",
               "Marian Kowalski","Ruch Narodowy",
               "#Kukiz","Paweł Kukiz","Kukiz","Kukiza","wilk", "wilka",
               "Dariusz Łaska","Łaska",
               "Nowicka","Wanda Nowicka","braun", "brauna",
               "kowalski", "kowalskiego", "kowalskiemu", "kowalskim",
               "#Ogórek","Magda Ogórek","Ogorek",
               "#Palikot", "Palikot", "Janusz Palikot",
               "Tanajno", "tanajno",
               "Jacek Wilk","Wilk",
               "Zydorczak",
               "prezydent", "prezydenta",
               "wybory","wybory prezydenckie",
               "kandydat","kandydata","kandydaci")

slownik<-slowa_klucz

library(stringi)
slownik <- stri_enc_toutf8(slownik)
