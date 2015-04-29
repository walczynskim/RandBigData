library(rvest)
library(XML)
library(stringi)

info <- data.frame(url = c("http://www.gazeta.pl", "http://www.dziennik.pl", "http://wyborcza.pl/0,0.html?piano_d=1"),
                   waga4 = c("//div[@class='col-md-8 col-sm-8 col-xs-12 mt_pict']//a", ".box-big-news-right a", ".cnt2 .content h3 a"),
                   waga3 = c("//div[@class='row mod_photo_box mod_section type_foreheads']//a", ".top h2 a", ".cnt3 .content a"),
                   waga2 = c(".box_news .news_box_pict a", ".topn h4 a", ".article #LinkArea\\:kraj"), 
                   waga1 = c(".box_news .news_box_links a", "#box_inside_sg .dbl_left .font_red", "#bottom_wrap .content a"))

parse <- function(lp_info){
  # zapamietuje date
  data <- as.character(Sys.Date()) 
  # zapisuje na zmienna url adres strony internetowej
  url <- info$url[lp_info]
  # ramka danych zawierajaca informacje wydobywane z danej strony
  df <- data.frame()
  # parsuje strone
  parsed_page <- htmlParse(url)

  ###############################################
  ### informacje na temat glownego artykulu; waga = 4 ###
  if(lp_info == 1){main_articles <- getNodeSet(parsed_page, info$waga4[lp_info])}
  else{main_articles <- html_nodes(parsed_page, info$waga4[lp_info])}
  
  # tytul glownego artykulu
  if(length(main_articles)>0){
    main_titles <- sapply(main_articles, xmlValue, "href")
    
    if(lp_info == 1){main_titles <- main_titles[1]}
    else if(lp_info == 2){main_titles <- main_titles[main_titles!=""]}
    
    main_titles <- stri_replace_all_regex(main_titles, ';', "")
    main_titles <- stri_trim_both(stri_replace_all_regex(main_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
    # linki do artykulu
    url_main_titles <- sapply(main_articles, xmlGetAttr, "href")
    url_main_titles <- url_main_titles[1]
    ktore <- sapply(main_titles, function(x) sprawdz_czy_wybory(slownik,x))
    if(sum(ktore)>0){
      df<-rbind(df, data.frame(date = data, portal = url, title = main_titles[ktore], position = 4, link = url_main_titles[ktore]))
    }
  }
  
  ###############################################
  ##informacje na temat drugiego artykulu; waga = 3 ###
  main2_articles <- getNodeSet(parsed_page, info$waga3[lp_info])
  # tytul 'drugich' artykulow
  if(lp_info == 1){
    main2_titles <- sapply(main2_articles, html_attr, "title")
    main2_titles <- unique(main2_titles)
  }
  else if(lp_info == 2){
    main2_titles <- sapply(main2_articles, html_attr, "title")
  }
  else{
    main2_titles <- xmlToDataFrame(main2_articles)
    main2_titles <- as.vector(main2_titles$text)
  }
  
  main2_titles <- stri_replace_all_regex(main2_titles, ';', "")
  main2_titles <- stri_trim_both(stri_replace_all_regex(main2_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
  # linki do artykulow
  url_main2_titles <- sapply(main2_articles, html_attr, "href")
  if( lp_info == 1){
    url_main2_titles <- unique(url_main2_titles)
    url_main2_titles <- unlist(sapply(url_main2_titles, function(x){ if(stri_extract_first_regex(x,".{4}")!="http"){ paste("http://",x, sep="")}else x}))
    names(url_main2_titles) <- NULL
  }
  ktore <- sapply(main2_titles, function(x) sprawdz_czy_wybory(slownik,x))
  if(sum(ktore)>0){
    df <- rbind(df, data.frame(date=data, portal = url, title = main2_titles[ktore], position=3, link = url_main2_titles[ktore]))
  }
   
  ###############################################
  ##informacja o 'prawych' artykulach; waga = 2 ###
  right_articles <- html_nodes(parsed_page, info$waga2[lp_info])
  # tytuly 'prawych' artykulow
  if( lp_info == 1){
    right_titles <- sapply(right_articles,html_attr,"title")
    right_titles <- unique(right_titles)
  }
  else if(lp_info == 2){
    right_titles <- sapply(right_articles,html_attr, "title")
  }
  else if(lp_info == 3){
    right_titles <- xmlToDataFrame(right_articles)
    right_titles <- as.vector(right_titles$text)
    right_titles <- right_titles[sapply(right_titles,function(x) x!="\n")]
  }
  
  right_titles <- stri_replace_all_regex(right_titles, ';', "")
  right_titles <- stri_trim_both(stri_replace_all_regex(right_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
  # linki do artykulow
  url_right_titles <- sapply(right_articles, html_attr, "href")
  if(lp_info == 1){
    url_right_titles <- unique(url_right_titles)
    url_right_titles <- unlist(sapply(url_right_titles, function(x){ if(stri_extract_first_regex(x,".{4}")!="http"){ paste("http://",x, sep="")}else x}))
    names(url_right_titles) <- NULL
  }
  else if(lp_info == 3){
    url_right_titles <- unique(url_right_titles)
  }
  ktore <- sapply(right_titles, function(x) sprawdz_czy_wybory(slownik,x))
  if(sum(ktore)>0){
    df <- rbind(df, data.frame(date = data, portal = url, title = right_titles[ktore], position = 2,link = url_right_titles[ktore]))
  } 
  
  ##informacja o innych artykulach, waga = 1 ###
  other_articles <- html_nodes(parsed_page, info$waga1[lp_info])
  # tytuly artykulow
  other_titles <- sapply(other_articles, html_attr, "title")
  if(lp_info == 1){other_titles <- unique(other_titles)}
  other_titles <- stri_replace_all_regex(other_titles, ';', "")
  other_titles <- stri_trim_both(stri_replace_all_regex(other_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
  # linki do artykulow
  url_other_titles <- sapply(other_articles, html_attr, "href")
  if(lp_info == 1){
    url_other_titles <- unique(url_other_titles)
    url_other_titles <- unlist(sapply(url_other_titles, function(x){ if(stri_extract_first_regex(x,".{4}")!="http"){ paste("http://",x, sep="")}else x}))
    names(url_main2_titles) <- NULL
  }
  ktore <- sapply(other_titles, function(x) sprawdz_czy_wybory(slownik,x))
  if(sum(ktore)>0){
    df<-rbind(df, data.frame(date=data, portal=url, title=other_titles[ktore], position=1,link=url_other_titles[ktore]))
  }
  
  if(dim(df)[1]>0){
    fname <- paste(getwd(), "/", data, ".csv", sep="")
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
  
}

# wywolanie
for(i in 1:nrow(info)){
  parse(i)
}