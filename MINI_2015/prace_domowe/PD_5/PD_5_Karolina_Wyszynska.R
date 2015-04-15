####################### PRACA DOMOWA - BAZY DANYCH #############################
library(stringi)

path <- "D://Projekty/wybory/"

#############  Zbieram dane z GoogleNews w ramke danych  #######################
#Wybieram tylko pierwsze 10 plikow
first10 <- list.files(paste0(path,"GoogleNews/"))[1:10]
daty <- unlist(stri_match_first_regex(first10,
                                "[0-9]+-[0-9]+-[0-9]+[ ][0-9]+-[0-9]+-[0-9]+"))
#Funkcja pomocnicza
kandydaci<-readLines(paste0(path,"slownik_fb_kandydaci.txt"))
tokeny <- readLines(paste0(path,"slownik_google_tokeny.txt"))
AnalizaPlik <- function(sciezka.plik){
  #Funkcja obliczajaca wskaĹşnik dla jednego czasu odczytu zliczeĹ„ z serwisu Google
  wystapienia <- character(0)
  try(wystapienia <- read.table(sciezka.plik,h=TRUE,encoding="UTF-8"), silent=TRUE)
  
  wskaznik <- numeric(length(kandydaci))
  names(wskaznik) <- kandydaci
  if(length(wystapienia)==0) return(wskaznik)
  
  #Poprawa struktury tabeli by spelniala wymagania wyjsciowego wektora
  imiona <- names(wystapienia)
  
  for(i in 1:ncol(wystapienia)){
    ktory <- which(stri_detect_regex(kandydaci, imiona[i]))
    wskaznik[ktory] <- wystapienia[[i]]
  }
  
  wskaznik
  
}

#Wkladam to do ramki danych
for(i in 1:seq_along(first10)){
  dane <- AnalizaPlik(paste0(path,"GoogleNews/",first10[i]))

  if(i==1){ ramkaDanych <- data.frame(t(dane));next}
  
  ramkaDanych <- rbind(ramkaDanych,data.frame(t(dane))) 
}
rownames(ramkaDanych) <- daty


###################### Przekazuje tabele do bazy danych ########################
library("RMySQL")
library(dplyr)
haslo <- '64tl1bp6un9sqxp8'


polaczenie <- src_mysql(dbname = 'students', 
                        host = 'beta.icm.edu.pl', user = 'pbiecek', password = haslo)
KWyszynska_GoogleNews <- ramkaDanych
db_drop_table(polaczenie$con, table="KW_GoogleNews")
ramkaDanychWBazie <- copy_to(polaczenie,KWyszynska_GoogleNews,temporary=FALSE)

#Sprawdzam czy ta tabela tam jest
sterownik <- MySQL()
mpolaczenie <- dbConnect(sterownik, 
                        user='pbiecek', password=haslo, dbname='students', 
                        host='beta.icm.edu.pl')
dbListTables(mpolaczenie)
dbDisconnect(mpolaczenie)













