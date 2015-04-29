### parsing fo gazeta.pl
parse_gazeta.pl <- function(){
  library(rvest)
  library(XML)
  library(stringi)
  # Date of publication:
  data <- as.character(Sys.Date())
  # Url of currently analysed site
  url <- "http://www.gazeta.pl"
  # Data frame containing all info about articles from specific HTML page:
  df <- data.frame()
  # Parsing an HTML page:
  parsed_page <- htmlParse(url)#,encoding="UTF-8")
  ###############################################
  ### Info about main articles; weight = 4 ###
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
  main2_titles <- unique(main2_titles)
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
  right_titles <- unique(right_titles)
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
  other_titles <- unique(other_titles)
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
parse_dziennik.pl <- function(){
  library(rvest)
  library(XML)
  library(stringi)
  # Date of publication:
  data <- as.character(Sys.Date())
  # Url of currently analysed site
  url <- "http://www.dziennik.pl"
  # Data frame containing all info about articles from specific HTML page:
  df <- data.frame()
  # Parsing an HTML page:
  parsed_page <- html(url)
  ###############################################
  ### Info about main articles; weight = 4 ###
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
  ##Infor about main2 articles; weight = 3 ###
  main2_articles <- html_nodes(parsed_page, ".top h2 a")
  # Titles of main articles:
  main2_titles <- sapply(main2_articles,html_attr, "title")
  main2_titles <- stri_replace_all_regex(main2_titles, ';', "")
  main2_titles <- stri_trim_both(stri_replace_all_regex(main2_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
  # Links to articles
  url_main2_titles <- sapply(main2_articles, html_attr, "href")
  ktore <- sapply(main2_titles, function(x) sprawdz_czy_wybory(slownik,x))
  if(sum(ktore)>0){
    df <- rbind(df, data.frame(date=data, portal=url, title=main2_titles[ktore], position=3,link=url_main2_titles[ktore]))
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
# parse dziennik.pl
parse_wyborcza.pl <- function(){
  library(rvest)
  library(XML)
  library(stringi)
  # Date of publication:
  data <- as.character(Sys.Date())
  # Url of currently analysed site
  url <- "http://wyborcza.pl/0,0.html?piano_d=1"
  # Data frame containing all info about articles from specific HTML page:
  df <- data.frame()
  # Parsing an HTML page:
  parsed_page <- html(url)
  ###############################################
  ### Info about main articles; weight = 4 ###
  main_articles <- html_nodes(parsed_page, ".cnt2 .content h3 a")
  #.cnt2 #LinkArea\\:MT a
  # Titles of main articles:
  if(length(main_articles)>0){
    main_titles <- sapply(main_articles, xmlValue, "href")
    main_titles <- stri_replace_all_regex(main_titles, ';', "")
    main_titles <- stri_trim_both(stri_replace_all_regex(main_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
    # Links to articles
    url_main_titles <- sapply(main_articles, xmlGetAttr, "href")
    ktore <- sapply(main_titles, function(x) sprawdz_czy_wybory(slownik,x))
    if(sum(ktore)>0){
      df<-rbind(df, data.frame(date=data, portal=url, title=main_titles[ktore], position=4,link=url_main_titles[ktore]))
    }
  }
  ###############################################
  ##Infor about main2 articles; weight = 3 ###
  main2_articles <- html_nodes(parsed_page, ".cnt3 .content a")
  # Titles of main articles:
  main2_titles <- xmlToDataFrame(main2_articles)
  main2_titles <- as.vector(main2_titles$text)
  main2_titles <- stri_replace_all_regex(main2_titles, ';', "")
  main2_titles <- stri_trim_both(stri_replace_all_regex(main2_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
  # Links to articles
  url_main2_titles <- sapply(main2_articles, html_attr, "href")
  ktore <- sapply(main2_titles, function(x) sprawdz_czy_wybory(slownik,x))
  if(sum(ktore)>0){
    df<-rbind(df, data.frame(date=data, portal=url, title=main2_titles[ktore], position=3,link=url_main2_titles[ktore]))
  }
  ###############################################
  ##Infor about right articles; weight = 2 ###
  right_articles <- html_nodes(parsed_page, ".article #LinkArea\\:kraj")
  # Titles of right articles:
  right_titles <- xmlToDataFrame(right_articles)
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