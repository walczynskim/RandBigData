#
PresidentDailyInfo <- function(dateArg=as.character(Sys.Date())) {
  require("dplyr")
  require("stringi")
  tempSitesNames <- c("http://www.tvp.info/", "http://www.wp.pl", "http://www.wprost.pl/",
                      "http://www.onet.pl/", "http://www.newsweek.pl/",
                      "http://www.tvn24.pl/", "http://natemat.pl/", "http://www.gazeta.pl",
                      "http://www.dziennik.pl", "http://wyborcza.pl/0,0.html?piano_d=1")
  # Data Frame with daily data about articles:
  dataPresidents <- read.table(paste0(dateArg,".csv"), sep = ";", 
                               stringsAsFactors = FALSE, header = TRUE) 
  dataPresidents <- dataPresidents[!duplicated(dataPresidents[,4:5]),]
  dataPresidents <- dataPresidents[!stri_detect_fixed(dataPresidents$portal,
                                                      'wpolityce'),]
  currentDate <- dateArg
  # Info on how many articles do the all sites provide for current day:
  dataElection <- table(dataPresidents[['portal']])
  dataElection <- dataElection[tempSitesNames]
  # Tags for searching
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
   
  fontSize <- data.frame("http://www.tvp.info/" = c(10, 18, 18, 22),
                         "http://www.wp.pl/" = c(18, 14, 21, 30),
                         "http://www.wprost.pl/" = c(17, 12, 28, 35),
                         "http://www.onet.pl/" = c(12, 12, 14, 31),
                         "http://www.newsweek.pl/" = c(21, 21, 21, 21),
                         "http://www.tvn24.pl/" = c(14, 14, 38, 46),
                         "http://natemat.pl/" = c(24, 24, 24, 42),
                         "http://www.gazeta.pl" = c(14, 20, 20, 30),
                         "http://www.dziennik.pl" = c(12, 22, 28, 74),
                         "http://wyborcza.pl/0,0.html?piano_d=1" = c(25, 20, 19, 30))
  names(fontSize) <- tempSitesNames
   
  # Candidates:
  candidateName <- c("komorowski", "duda", "ogorek", "korwin", "kukiz",
                     "jarubas", "palikot", "wilk", "braun", "tanajno")
   
  # Articles' analysis for all candidates:
  for (i in candidateName) {
  # Checking which president do we analyse now:
    switch(i,
           komorowski={ 
              tagsTemp <- tagsKomorowski
           },
           duda = { 
              tagsTemp <- tagsDuda
           },
           ogorek = {
              tagsTemp <- tagsOgorek
           },
           korwin = {
              tagsTemp <- tagsKorwin
           },
           kukiz = {
              tagsTemp <- tagsKukiz
           },
           jarubas = {
              tagsTemp <- tagsJarubas
           },
           palikot = {
              tagsTemp <- tagsPalikot
           },
           wilk = {
              tagsTemp <- tagsWilk
           },
           braun = {
              tagsTemp <- tagsBraun
           },
           kowalski = {
              tagsTemp <- tagsKowalski
           },
           tanajno = {
              tagsTemp <- tagsTanajno
           }
    )
    # Dividing titles into parts for further analyse:
    titlesDivided <- stri_extract_all_words(stri_trans_tolower(dataPresidents$title))
    # Checking which articles are about demanded candidate:
    whichArticle <- lapply(titlesDivided, FUN = function(x) {
       any(tagsTemp %in% x)
    }) %>%
       unlist(.)
    # Artcles about chosen candidate:
    dataPresidentsChosen <- dataPresidents[whichArticle, ]
      
    # File #1 provides info on how many articles was the current candidate mentioned:
    dataNew <- data.frame()
    links <- tempSitesNames
    dataNew <- as.data.frame(lapply(links, FUN = function(x) { 
       sum(dataPresidentsChosen$portal == x)
       }))
    names(dataNew) <- c("TvPInfo", "WpPl", "WprostPl", "OnetPl", "NewsweekPl",
                        "Tvn24Pl", "NatematPl", "GazetaPl", "DziennikPl", "WyborczaPl")
    dataNew$date <- currentDate
    dataNew$candidateName <- i
    
    colnamesCandidate <- paste('\"TvPInfo\"', '\"WpPl\"', '\"WprostPl\"',
                               '\"OnetPl\"', '\"NewsweekPl\"', '\"Tvn24Pl\"',
                               '\"NatematPl\"','\"GazetaPl\"','\"DziennikPl\"',
                               '\"WyborczaPl\"', '\"date\"','\"surname\"', 
                               sep = ";")
    fname <- paste0(getwd(), "2\\", "candidate_general_info", ".csv")
    if (!file.exists(fname)) {
       f <- file(fname, open="a", encoding="UTF-8")
       #tworze pierwszy wiersz w pliku:
       writeLines(colnamesCandidate, f)
    } else {
       f <- file(fname, open="a", encoding="UTF-8")
    }   
    paste(dataNew[1,], collapse=";") %>%
       writeLines(., f)
    close(f)
     
    # File #2, for main articles:
    dataNew2 <- dataPresidentsChosen[dataPresidentsChosen$position == 4,]
    if (nrow(dataNew2) > 0) {
      dataNew2$fontSize <- unlist(fontSize[4, dataNew2$portal, drop=TRUE])
      dataNew2$candidateName <- i
      PositionWriteLines(4, dataNew2)
    }
     
    # FILE #3 for submain articles:
    dataNew3 <- dataPresidentsChosen[dataPresidentsChosen$position == 3, ]
    if (nrow(dataNew3) > 0) {
      dataNew3$fontSize <- unlist(fontSize[3, dataNew3$portal, drop=TRUE])
      dataNew3$candidateName <- i
      PositionWriteLines(3,dataNew3)
    }
    
    # FILE #4 for side articles:
    dataNew4 <- dataPresidentsChosen[dataPresidentsChosen$position == 2, ]
    if (nrow(dataNew4) > 0) {
      dataNew4$fontSize <- unlist(fontSize[2, dataNew4$portal, drop=TRUE])
      dataNew4$candidateName <- i
      PositionWriteLines(2,dataNew4)
    }
    
    # FILE #5 for remaining articles:
    dataNew5 <- dataPresidentsChosen[dataPresidentsChosen$position == 1, ]
    if (nrow(dataNew5) > 0) {
      dataNew5$fontSize <- unlist(fontSize[1, dataNew5$portal, drop=TRUE])
      dataNew5$candidateName <- i
      PositionWriteLines(1,dataNew5)
    }
    # FILE #6 fraction post about candidate to all posts
    # dataNew6 <- data.frame()
    dataNew6 <-  round( dataNew[1,1:10] / as.vector(dataElection), 4)
    dataNew6$date <- currentDate
    dataNew6$candidateName <- i
     
    fname <- paste0(getwd(), "2\\", "candidate_fraction_info", ".csv")
    if (!file.exists(fname)) {
       f <- file(fname, open="a", encoding="UTF-8")
       #tworze pierwszy wiersz w pliku:
       writeLines(colnamesCandidate, f)
    } else {
       f <- file(fname, open="a", encoding="UTF-8")
    }   
    paste(dataNew6[1,], collapse=";") %>%
      writeLines(., f)
    close(f)
  }
  return(invisible(NULL))
}

PositionWriteLines <- function(position, dataFrame) {
  # Function writes into .csv according to position of analysed articles.
   rowNumber <- nrow(dataFrame)
   if (rowNumber == 0) return(invisible(NULL))
   fname <- paste0(getwd(), "2\\", "candidate_position", ".csv")
   if (!file.exists(fname)){
      f <- file(fname, open="a")
      writeLines(stri_paste('\"date\"', '\"portal\"', '\"title\"', '\"position\"',
                            '\"link\"', '\"fontSize\"','\"Nazwisko\"',
                            sep = ";"), f)
   } else {
      f <- file(fname, open="a")  
   }
   for(i in 1:rowNumber) {
   paste(dataFrame[i, ], collapse=";") %>%
      writeLines(., f)
   }
   close(f)
   return(invisible(NULL))
}
