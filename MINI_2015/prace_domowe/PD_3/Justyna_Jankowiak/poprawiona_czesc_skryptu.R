library(stringi)
#funkcje wspólne
poczatek <- function(url){
   library(rvest)
   library(XML)
   
   # Date of publication:
   data <- as.character(Sys.Date())
   # Data frame containing all info about articles from specific HTML page:
   df <- data.frame()
   # Parsing an HTML page:
   parsed_page <- htmlParse(url)
   return(list(data, df, parsed_page))
}

ktore_do_ramki <- function(data, url, tytuly, url_tytuly, df0, pozycja){
   ktore <- sapply(tytuly, function(x) sprawdz_czy_wybory(slownik,x))   
   if(sum(ktore)>0){
      df <- rbind(df0, data.frame(date=data, portal=url, title=tytuly[ktore], position=pozycja, link=url_tytuly[ktore])) 
      return(df)
   } else {
      return(df0)
   }  
}

tytuly_i_linki <- function(typ_art){
   # Titles of main2 articles:
   typ <- sapply(typ_art,html_attr, "title")
   typ <- unique(typ)
   typ <- stri_replace_all_regex(typ, ';', "")
   typ <- stri_trim_both(stri_replace_all_regex(typ,"(\\n)|(\\t)|(\\r)|(\")"," "))
   
   # Links to articles
   url_typ <- sapply(typ_art, html_attr, "href")
   url_typ <- unique(url_typ)
   url_typ <- unlist(sapply(url_typ, function(x){ if(stri_extract_first_regex(x,".{4}")!="http"){ paste("http://",x, sep="")}else x}))
   names(url_typ) <- NULL
   return(list(typ, url_typ))
}

tytuly_i_linki_main <- function(main_articles){
   # Titles of main articles:
   if(length(main_articles)>0){
      
      main_titles <- sapply(main_articles, xmlValue, "href")
      main_titles <- main_titles[1]
      main_titles <- stri_replace_all_regex(main_titles, ';', "")
      main_titles <- stri_trim_both(stri_replace_all_regex(main_titles,"(\\n)|(\\t)|(\\r)|(\")"," "))
      
      # Links to articles
      url_main_titles <- sapply(main_articles, xmlGetAttr, "href")
      url_main_titles <- url_main_titles[1]
      return(list(main_titles, url_main_titles))
      } 
}

zapisywanie <- function(df, data){
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
}

sprawdz_czy_wybory <- function(slownik, tekst){
   
   tekst <- stri_trans_tolower(tekst)
   tmp <- unlist(stri_extract_all_words(tekst))
   res <- any(tmp%in%slownik)
   
   return(res)
   
}

slownik <- c("bronisław","bronisława", "komorowski","komorowskiego","komorowskiemu",
             "bronislaw",
             " komorowskiemu","komorowskich","komorowscy",
             "duda","dudy","dudę","dude","dudą","dudzie",
             "korwin-mikkego","dudzie","korwin-mikke", "korwin","korwina","jkm",
             "wybory","wyborach" ,"prezydent","prezydenta", "prezydenckie",
             "kampania","kampanią","kampanii","sondaż","pkw","ogórek","ogorek", "kukiz",
             "jarubas","jarubasa","palikot","palikota","anna grodzka","anny grodzkiej",
             "annę grodzką","annie grodzkiej","marian kowalski","mariana kowalskiego",
             "wanda nowicka","wandy nowickiej","kukiz","kukiza","palikot","palikota")

slownik <- stri_encode(slownik,to="UTF-8")



parse_gazeta.pl <- function(){
   url <- "http://www.gazeta.pl"
   data <- poczatek(url)[[1]]
   df0 <- poczatek(url)[[2]]
   parsed_page <- poczatek(url)[[3]]

   ###############################################
   ### Info about main articles;   weight = 4 ###
   main_articles <- getNodeSet(parsed_page, "//div[@class='col-md-8 col-sm-8 col-xs-12 mt_pict']//a")
   main_titles <- tytuly_i_linki_main(main_articles)[[1]]
   url_main_titles <- tytuly_i_linki_main(main_articles)[[2]]
   df1 <- ktore_do_ramki(data, url, main_titles, url_main_titles, df0, 4)
   
   ###############################################
   ##Infor about main2 articles; weight = 3 ###
   
   main2_articles <- getNodeSet(parsed_page, "//div[@class='row mod_photo_box mod_section type_foreheads']//a")
   main2_titles <- tytuly_i_linki(main2_articles)[[1]]
   url_main2_titles <- tytuly_i_linki(main2_articles)[[2]]
   df2 <- ktore_do_ramki(data, url, main2_titles, url_main2_titles, df1, 3)

   ###############################################
   ##Infor about right articles; weight = 2 ### 
   
   right_articles <- html_nodes(parsed_page, ".box_news .news_box_pict a")
   right_titles <- tytuly_i_linki(right_articles)[[1]]
   url_right_titles <- tytuly_i_linki(right_articles)[[2]]
   df3 <- ktore_do_ramki(data, url, right_titles, url_right_titles, df2, 2)
   
   ###############################################
   ##Infor about other aticles, weight = 1 ###
   
   other_articles <- html_nodes(parsed_page, ".box_news .news_box_links a")
   other_titles <- tytuly_i_linki(other_articles)[[1]]
   url_other_titles <- tytuly_i_linki(other_articles)[[2]] 
   df4 <- ktore_do_ramki(data, url, other_titles, url_other_titles, df3, 1)
   zapisywanie(df4, data)
}

parse_dziennik.pl <- function(){
   url <- "http://www.dziennik.pl"
   data <- poczatek(url)[[1]]
   df0 <- poczatek(url)[[2]]
   parsed_page <- poczatek(url)[[3]]
   
   ###############################################
   ### Info about main articles;   weight = 4 ###
   main_articles <- html_nodes(parsed_page, ".box-big-news-right a")
   main_titles <- tytuly_i_linki_main(main_articles)[[1]]
   url_main_titles <- tytuly_i_linki_main(main_articles)[[2]]
   if(length(main_articles)!=0){
      df1 <- ktore_do_ramki(data, url, main_titles, url_main_titles, df0, 4)
   } else {
      df1 <- df0
   }
   
   ###############################################
   ##Infor about main2  articles; weight = 3 ###
   
   main2_articles <- html_nodes(parsed_page, ".top h2 a")
   main2_titles <- tytuly_i_linki(main2_articles)[[1]]
   url_main2_titles <- tytuly_i_linki(main2_articles)[[2]]    
   df2 <- ktore_do_ramki(data, url, main2_titles, url_main2_titles, df1, 3)
   
   ###############################################
   ##Infor about right articles; weight = 2 ###
   
   right_articles <- html_nodes(parsed_page, ".topn h4 a")
   right_titles <- tytuly_i_linki(right_articles)[[1]]
   url_right_titles <- tytuly_i_linki(right_articles)[[2]]
   df3 <- ktore_do_ramki(data, url, right_titles, url_right_titles, df2, 2)
   
   ###############################################
   ##Infor about other articles; weight = 1 ###
   
   other_articles <- html_nodes(parsed_page, "#box_inside_sg .dbl_left .font_red")
   other_titles <- tytuly_i_linki(other_articles)[[1]]
   url_other_titles <- tytuly_i_linki(other_articles)[[2]]
   df4 <- ktore_do_ramki(data, url, other_titles, url_other_titles, df3, 1)

   zapisywanie(df4, data)  
}

parse_gazeta.pl()
parse_dziennik.pl()
