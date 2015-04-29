setwd("D:\\Moje dokumenty\\Emilka\\SMAD\\RAndBigData\\P1")
#FUNKCJA ROBI SIE RAZ, A POTEM CODZIENNIE SIE DOPISUJE NOWE WARTOSCI - TRZEBA PUSZCZAC 
#CODZIENNIE

sentyment_tweety <- function(){
  
  library(streamR)
  library(dplyr)
  library(stringi)
  
  source(file = "F:\\doc\\R i Big Data\\Rproject1\\sentyment.R")
  source(file = "F:\\doc\\R i Big Data\\Rproject1\\oczyszczanie.R")
  
  Parsed <- data.frame()
  
  data <- Sys.Date()
  fname <- "Podsumowanie_tweetow.csv"
  
  if(!file.exists(fname)){
    f <- file(fname, open="a")
    #tworze pierwszy wiersz w pliku:
    writeLines(stri_paste('\"lastname\"', '\"date\"', '\"sentiment\"', sep = ";"), f)
    
    pliki <- list.files(pattern="R_project1_tweets-")
    N <- length(pliki)
    
    for(i in 1:N){
      
      Parsed <- Parsed %>%
        rbind(parseTweets(pliki[i], simplify = FALSE, verbose = TRUE))
    }
  } else{
    
      f <- file(fname, open="a")
      plik <- stri_paste("R_project1_tweets-",as.character(data),".json")
      if(!file.exists(plik)){
        close(f)
        return(invisible(NULL))
      }
      
      Parsed <- Parsed %>%
      rbind(parseTweets(plik, simplify = FALSE, verbose = TRUE))
      
    }
  
  Parsed

  #BIERZEMY TRESCI TWEETOW - BEDZIEMY JE UPRASZCZAC
  tresci <- Parsed[,1]
  
  modified <- oczyszczanie(tresci)
  
  #BEDZIEMY SPRAWDZAC KTORY TWEET KOGO DOTYCZY
  tagsKomorowski <- c("bronisław", "bronisława", "bronisławowi", "bronisława", 
                      "bronisławem", "bronisławie", "bronisławie", "bronislaw",
                      "bronislawa", "bronislawowi","bronislawem","komorowski",
                      "komorowskiego", "komorowskiemu", "komorowskiego", 
                      "komorowskim", "komorowskim", "komorowski", "bronkobus",
                      "bronkobusa") 
  tagsDuda <- c("andrzej", "andrzeja", "andrzejowi", "andrzeja", "andrzejem",
                "andrzeju", "duda", "dudy", "dudzie", "dudę", "dudą",
                "dudzie", "dudo","dude")
  tagsOgorek <- c("magdalena", "magdaleny", "magdalenie", "magdalenę",
                  "magdaleną", "magdalenie", "magdaleno", "ogórek", "ogorek")
  tagsKorwin <- c("janusz", "janusza", "januszowi", "januszem", "januszu",
                  "korwin-mikke", "korwin", "mikke", "jkm")
  tagsKukiz <- c("kukiz", "kukiza")
  tagsJarubas <- c("jarubas", "jarubasa")
  tagsPalikot <- c("palikot", "palikota")
  tagsWilk <- c("wilk", "wilka")
  tagsBraun <- c("braun", "brauna")
  tagsKowalski <- c("kowalski", "kowalskiego", "kowalskiemu", "kowalskim")
  tagsTanajno <- c("tanajno")
  
  
  kandydaci <- list(komorowski=tagsKomorowski,duda=tagsDuda,korwin=tagsKorwin,ogorek=tagsOgorek,
                    jarubas=tagsJarubas, kukiz=tagsKukiz, palikot=tagsPalikot,
                    wilk=tagsWilk, braun=tagsBraun, kowalski= tagsKowalski, tanajno=tagsTanajno)
  
  k <- length(kandydaci)
  
  for(i in 1:k){
    
    Parsed<-cbind(Parsed,unlist(lapply(modified,function(y){
      any(y%in%kandydaci[[i]])
    })))
    
  }
  
  names(Parsed)[43:53]<-names(kandydaci)
  
  wartosci <- sentyment(modified)
  
  Parsed<-tbl_df(Parsed)
  
  Parsed <- cbind(Parsed,wartosci)
  
  Sys.setlocale("LC_TIME", "English")
  
  dat<-as.character(strptime(Parsed$created_at,format="%a %b %d %H:%M:%S %z %Y"))
  
  Parsed <- Parsed%>%
    mutate(day=unlist(stri_extract_all_regex(dat, "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}")))
  
  lapply(names(kandydaci), function(y){
    
    dane <- Parsed %>%
      filter(Parsed[,y]==TRUE) %>%
      group_by(day) %>%
      summarise(sentyment=sum(wartosci))
    
    dane<-as.data.frame(dane)
    ile <- nrow(dane)
    if(ile>0){
      for(i in 1:ile){
        
        writeLines(stri_paste(y, dane[i,1], dane[i,2], sep=";"),f)
      }
    }
    
  })
  close(f)
  
}

sentyment_tweety()
