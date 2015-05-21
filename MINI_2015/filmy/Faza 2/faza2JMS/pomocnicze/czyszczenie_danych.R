setwd("D:/GoogleDrive/Studia/SemestrVIII/R-i-Big-Data/Filmy/dane/")

library(stringi)
library(tm)
library(dplyr)
library(SnowballC)

###########################################################################
##### ACTORS #####

actors <- readLines("Actors.csv")

#jako argument mamy plik z readLines, czyli wektor napisów

wyczysc_aktorow <- function(actors) {
   #usuwamy duplikaty (strasznie duuużo!)
   a <- unique(actors)
   a <- stri_split_fixed(a, ";")
   #w niektórych jest jedna dodatkowa kolumna na końcu, więc usuwamy
   a <- lapply(a, function(x){
      if(length(x)==6) x <- x[-length(x)]
      else x <- x
   })
   
   #nazwy kolumn - poprawka, bo nazwa miejsca urodzenia jest zamieniona z datą urodzenia
   head <- a[[1]]
   place <- head[3]
   head[3] <- head[4] 
   head[4] <- place
   
   n <- length(a)
   df_a <- data.frame(matrix(unlist(a), nrow = n, byrow = T),
                      stringsAsFactors=FALSE)
   colnames(df_a) <- head
   df <- df_a[-1,]
   
   #zamiast całej daty bierzemy tylko rok - to wystarczy
   #zwłaszcza, że dla niektórych mamy tylko sam rok podany
   df[,3] <- strftime(strptime(df[,3], "%Y"), "%Y")
   df[,5] <- strftime(strptime(df[,5], "%Y"), "%Y")
   df[is.na(df)] <- "NA"
   
   #usuwamy wiersze, gdzie o aktorze nie ma żadnej informacji 
   #tzn. kolumny 3, 4, 5 są puste
   
   zostaja <- which(!(df[,3] == "NA" & df[,4] == "NA" & df[,5] == "NA"))
   df2 <- df[zostaja,]
   return(df2)
}

df2 <- wyczysc_aktorow(actors)

#zapisujemy
nazwa <- "ActorsClean.csv"
f <- file(nazwa, open = "a")
writeLines(stri_paste(colnames(df2), collapse = ";"), f)
for (i in 1:nrow(df2)) {
   writeLines(stri_paste(df2[i,1], df2[i,2], df2[i,3], df2[i,4], df2[i,5], sep=";"), f)
}  
close(f)


###########################################################################
##### DIRECTORS #####

directors <- readLines("Director.csv")

#jako argument mamy plik z readLines, czyli wektor napisów

wyczysc_rezyserow <- function(directors) {
   d <- unique(directors)
   d <- stri_split_fixed(d, ";")
   #w niektórych jest jedna dodatkowa kolumna na końcu, więc usuwamy
   d <- lapply(d, function(x){
      if(length(x)==6) x <- x[-length(x)]
      else x <- x
   })
   
   #nazwy kolumn - poprawka, bo nazwa miejsca urodzenia jest zamieniona z datą urodzenia
   head <- d[[1]]
   place <- head[3]
   head[3] <- head[4] 
   head[4] <- place
   
   n <- length(d)
   df_d <- data.frame(matrix(unlist(d), nrow = n, byrow = T),
                      stringsAsFactors=FALSE)
   colnames(df_d) <- head
   df <- df_d[-1,]
   
   #zamiast całej daty bierzemy tylko rok - to wystarczy
   #zwłaszcza, że dla niektórych mamy tylko sam rok podany
   df[,3] <- strftime(strptime(df[,3], "%Y"), "%Y")
   df[,5] <- strftime(strptime(df[,5], "%Y"), "%Y")
   df[is.na(df)] <- "NA"
   
   #usuwamy wiersze, gdzie o reżyserze nie ma żadnej informacji 
   #tzn. kolumny 3, 4, 5 są puste
   zostaja <- which(!(df[,3] == "NA" & df[,4] == "NA" & df[,5] == "NA"))
   df2 <- df[zostaja,]
   return(df2)
}

df2 <- wyczysc_rezyserow(directors)
#zapisujemy
nazwa <- "DirectorsClean.csv"
f <- file(nazwa, open = "a")
writeLines(stri_paste(colnames(df2), collapse = ";"), f)
for (i in 1:nrow(df2)) {
   writeLines(stri_paste(df2[i,1], df2[i,2], df2[i,3], df2[i,4], df2[i,5], sep=";"), f)
}  
close(f)


###########################################################################
##### RECENZJE #####

review <- readLines("recenzje.csv")

#jako argument mamy plik z readLines, czyli wektor napisów

wyczysc_recenzje <- function(review) {
   r <- unique(review)
   r <- stri_split_fixed(r, ";")
   r <- lapply(r, function(x){
      if(length(x)==3) x <- x[-length(x)]
      else x <- x
   })
   head <- r[[1]]
   n <- length(r)
   df_r <- data.frame(matrix(unlist(r), nrow = n, byrow = T),
                      stringsAsFactors=FALSE)
   colnames(df_r) <- head
   df <- df_r[-1,]
   
   #usuwamy te, które mają tylko id, ale recenzja jest pusta
   na <- which(df[,2]!="NA")
   df <- df[na,]
   
   #czyszcze treści
   corpus <- Corpus(VectorSource(df[,2]))
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, content_transformer(tolower))
   corpus <- tm_map(corpus, removeWords, stopwords("english"))
   corpus <- tm_map(corpus, stemDocument)
   corpus <- tm_map(corpus, stripWhitespace)
   
   df2 <- data.frame(id = df[,1], text=unlist(sapply(corpus, `[`, "content")),
                     stringsAsFactors = FALSE)
   
   #niestety mogą być jakieś bugi, w wierszach
   ktore.ok <- stri_detect_regex(df2[,1], "tt[0-9]+")
   df2 <- df2[ktore.ok, ]
   
   #łącze recenzje - jeden wiersz dla jednego filmu (połączone recenzje wg id)
   df2[,1] <- as.factor(df2[,1])
   lista <- tapply(df2[,2], df2[,1], function(x){
      stri_paste(x, collapse = " ")
   })
   
   text <- unlist(lista)
   slowa <- stri_extract_all_words(text)
   slowa <- lapply(slowa, function(x) stri_unique(x))
   text2 <- lapply(slowa, function(x) paste(x, collapse = " "))
   df3 <- data.frame(id = names(lista), text = unlist(text2))
   #w razie jakby jakimś cudem były dwa filmy o tym samym id ale nie identyczne w innych kolumnach
   df3 <- df3[!duplicated(df3[,1]),]
   return(df3)
}

df3 <- wyczysc_recenzje(review)
#zapisujemy
nazwa <- "recenzjeClean.csv"
f <- file(nazwa, open = "a")
writeLines(stri_paste(colnames(df3), collapse = ";"), f)
for (i in 1:nrow(df3)) {
   writeLines(stri_paste(df3[i,1], df3[i,2], sep=";"), f)
}  
close(f)


###########################################################################
##### FILMY #####

films <- readLines("filmy.csv")

wyczysc_filmy <- function(films) {
   f <- unique(films)
   f <- stri_split_fixed(f, ";")
   
   #patrzymy, które się źle zapisały, tzn. w opisie były średniki i kolumny sę podzieliły
   dobre <- sapply(f, function(x){
      length(x)==24
   })
   
   f <- f[dobre]
   
   #może być tak, że po 24 kolumnie są też puste więc je kasujemy
   f <- lapply(f, function(x){
      if(length(x)!=24) x <- x[1:24]
      else x <- x
   })
   head <- f[[1]]
   n <- length(f)
   df_f <- data.frame(matrix(unlist(f), nrow = n, byrow = T),
                      stringsAsFactors=FALSE)
   colnames(df_f) <- head
   df <- df_f[-1,]
   
   #czyszczę niektóre kolumny
   #Time - wystarczy liczba, wszystko jest w minutach
   czas <- stri_replace_all_fixed(df[,5], "min", "")
   czas <- stri_trim_both(czas)
   df[,5] <- czas
   
   #Reviews - też zostawiamy tylko liczbę
   rev <- stri_replace_all_regex(df[,14], "user", "")
   rev <- stri_trim_both(rev)
   df[,14] <- rev
   
   #czyszcze kolumny opisowe
   corpus <- Corpus(VectorSource(df[,8]))
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, content_transformer(tolower))
   corpus <- tm_map(corpus, removeWords, stopwords("english"))
   corpus <- tm_map(corpus, stemDocument)
   corpus <- tm_map(corpus, stripWhitespace)
   story <- unlist(sapply(corpus, `[`, "content"))
   slowa <- stri_extract_all_words(story)
   slowa <- lapply(slowa, function(x) stri_unique(x))
   text2 <- lapply(slowa, function(x) paste(x, collapse = " "))
   text2 <- lapply(text2, function(x) stri_replace_all_regex(x, "na", "NA"))
   
   corpus <- Corpus(VectorSource(df[,17]))
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, content_transformer(tolower))
   corpus <- tm_map(corpus, removeWords, stopwords("english"))
   corpus <- tm_map(corpus, stemDocument)
   corpus <- tm_map(corpus, stripWhitespace)
   desc <- unlist(sapply(corpus, `[`, "content"))
   slowa <- stri_extract_all_words(desc)
   slowa <- lapply(slowa, function(x) stri_unique(x))
   text2 <- lapply(slowa, function(x) paste(x, collapse = " "))
   text2 <- lapply(text2, function(x) stri_replace_all_regex(x, "na", "NA"))
   
   df[,8] <- story
   df[,17] <- desc
   
   #w razie jakby jakimś cudem były dwa filmy o tym samym id ale nie identyczne w innych kolumnach
   df <- df[!duplicated(df[,1]),]
   return(df)
}

df <- wyczysc_filmy(films)

#zapisujemy
nazwa <- "filmyClean.csv"
f <- file(nazwa, open = "a")
#nie biore 12 kolumny z max_rating bo wszedzei jest 10
writeLines(stri_paste(colnames(df)[-12], collapse = ";"), f)
for (i in 1:nrow(df)) {
   writeLines(stri_paste(df[i,1], df[i,2], df[i,3], df[i,4], df[i,5], df[i,6], df[i,7],
                         df[i,8], df[i,9], df[i,10], df[i,11], df[i,13], df[i,14], df[i,15],
                         df[i,16], df[i,17], df[i,18], df[i,19], df[i,20], df[i,21],
                         df[i,22],
                         df[i,23],
                         df[i,24],sep=";"), f)
}  
close(f)
