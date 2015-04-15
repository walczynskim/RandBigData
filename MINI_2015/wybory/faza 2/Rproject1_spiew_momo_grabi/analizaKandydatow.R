setwd("D:\\Moje dokumenty\\Emilka\\SMAD\\RAndBigData\\P1\\wybory")

Candidates <- function(){
  
  library(PogromcyDanych)
  library(dplyr)
  library(stringi)
  
  data <- Sys.Date()
  fname <- ("Podsumowanie_nasluch_kandydatow.csv")
  
  
  if(!file.exists(fname)){
    f <- file(fname, open="a")
    #tworze pierwszy wiersz w pliku:
    writeLines(stri_paste('\"lastname\"', '\"date\"', '\"mean_retweet\"', '\"sum_retweet\"',
                          '\"mean_like\"', '\"sum_like\"', '\"how_many_tweets\"', sep = ";"), f)
    pliki <- list.files(, pattern="tweety_kandydatow")
    n <- length(pliki)
    if(n==0){
      close(f)
      return(invisible(NULL))
    }
    
    a <- data.frame()
    
    for(i in 1:n){
      a <- rbind(a, read.csv2(pliki[i]))
    }
  }else{
    f <- file(fname, open="a")
    plik <- stri_paste("tweety_kandydatow_",as.character(data),".csv")
    if(!file.exists(plik)){
      close(f)
      return(invisible(NULL))
    }
    a <- read.csv2(plik)
  }
  a <- tbl_df(a)
  
  #WYRZUCAM DUPLIKATY, ZOSTAWIAM TE POZNIEJSZE, BO ONE SA BARDZIEJ AKTUALNE
  duplikaty <- which(duplicated(a[,1], fromLast=TRUE))
  if(length(duplikaty)>0){
      a <- a[-which(duplicated(a[,1], fromLast=TRUE)),]
  }
  a <- unique(a)
  
  day=unlist(stri_extract_all_regex(a$created, "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}"))
  #DODAJEMY KOLUMNE DATA - BEZ GODZINY, ZEBY MOC GRUPOWAC PO TYM
  nowy <- a%>%
    mutate(day=unlist(stri_extract_all_regex(created, "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}")))
  
  #PODSUMOWANIE
  podsumowanie_dni <- nowy%>%
    group_by(lastname, day)%>%
    summarise(srednio_retweetow=mean(retweetCount),
              suma_retweetow=sum(retweetCount),
              srednio_like=mean(favoriteCount),
              suma_like=sum(favoriteCount),
              liczba_tweetow=n())
  podsumowanie_dni <- as.data.frame(podsumowanie_dni)
  n<-nrow(podsumowanie_dni)
  if(n!=0){
    for(i in 1:n){
      #dopisuje do pliku kolejny wiersz      
      writeLines(stri_paste(podsumowanie_dni[i,1], podsumowanie_dni[i,2], podsumowanie_dni[i,3],
                            podsumowanie_dni[i,4], podsumowanie_dni[i,5],podsumowanie_dni[i,6],
                            podsumowanie_dni[i,7], sep=";"),f)
    }
  }

  
  close(f)
  
}



