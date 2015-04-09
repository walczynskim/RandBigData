library("stringi")
library("dplyr")
library("tm")
library("wordcloud")
library("RCurl")
library("XML")
library("rvest")
library("ggplot2")
library("gridExtra")

###############################################################
# zapisanie twittow do jednego wspolnego pliku:
###############################################################

# folder_with_twitts <- "dane\\twitter\\"
# files_txt <- list.files(folder_with_twitts, pattern = ".*tabela.*txt$", full.names = TRUE)
# 
# gdzie <- file("analiza//twitter//jeden_wspolny.txt", "a")
# for(i in 1:length(files_txt)){
#    one_file_directory <- files_txt[i] 
#    tekst <- readLines(one_file_directory)[-1]
#    writeLines(tekst, gdzie)
# }   
# close(gdzie)

###############################################################
# stworzenie tabeli ze wszystkich twittow:
###############################################################

# one_file_directory <- "analiza//twitter//jeden_wspolny.txt"
# 
# tekst <- readLines(one_file_directory)[-1]
# 
# tekst <- stri_replace_all_fixed(tekst, "\"", "")
# 
# ile_gwiazdek <- stri_count_fixed(tekst, "*")
# 
# dobry_tekst <- character()
# k <- 1
# wiersz <- ""
# znacznik <- 0
# suma <- 0
# 
# for(i in 1:length(ile_gwiazdek)){
#    if(ile_gwiazdek[i]>=16 && suma==0){
#       dobry_tekst[k] <- tekst[i]
#       k <- k+1
#    } else{
#       wiersz <- stri_paste(wiersz, tekst[i])
#       suma <- suma+ile_gwiazdek[i]
#       if(suma >= 16){
#          dobry_tekst[k] <- wiersz
#          k <- k+1
#          wiersz <- ""
#          suma <- 0
#       }
#    }
# }
# 
# oddzielone <- stri_split_fixed(dobry_tekst, "*")
# tabela <- do.call(rbind.data.frame, oddzielone)
# 
# colnames(tabela) <- c("row_name", "text", "favorited", "favoriteCount", "replyToSN",
#                       "created", "truncated", "replyToSID", "id", "replyToUID",
#                       "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted",
#                       "longitude", "latitude") 
# 
# write.table(tabela, "analiza//twitter//tabela.txt", sep="***moj_cudowny_separator***",
#             quote = FALSE)

###############################################################
# wczytanie tabeli:
###############################################################

cala_tabela <- data.frame(do.call(rbind,
                                  strsplit(readLines("indicators//twitter//tabela.txt")[-1],
                                           "***moj_cudowny_separator***",
                                           fixed=TRUE))[,-c(1,2)])

colnames(cala_tabela) <- c("text", "favorited", "favoriteCount", "replyToSN",
                           "created", "truncated", "replyToSID", "id", "replyToUID",
                           "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted",
                           "longitude", "latitude") 

