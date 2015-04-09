library("stringi")
library("dplyr")
library("tm")
library("wordcloud")
library("RCurl")
library("XML")
library("rvest")
library("ggplot2")
library("gridExtra")

folder_with_twitts <- "dane\\twitter\\"
files_txt <- list.files(folder_with_twitts, pattern = ".*tabela.*txt$", full.names = TRUE)

for(i in 1:length(files_txt)){
   
   one_file_directory <- files_txt[i]
   
   tekst <- readLines(one_file_directory)[-1]
   
   tekst <- stri_replace_all_fixed(tekst, "\"", "")
   
   ile_gwiazdek <- stri_count_fixed(tekst, "*")
   
   dobry_tekst <- character()
   k <- 1
   wiersz <- ""
   znacznik <- 0
   suma <- 0
   
   for(i in 1:length(ile_gwiazdek)){
      if(ile_gwiazdek[i]>=16 && suma==0){
         dobry_tekst[k] <- tekst[i]
         k <- k+1
      } else{
         wiersz <- stri_paste(wiersz, tekst[i])
         suma <- suma+ile_gwiazdek[i]
         if(suma >= 16){
            dobry_tekst[k] <- wiersz
            k <- k+1
            wiersz <- ""
            suma <- 0
         }
      }
   }
   
   
   oddzielone <- stri_split_fixed(dobry_tekst, "*")
   tabela <- do.call(rbind.data.frame, oddzielone)
   
   colnames(tabela) <- c("row_name", "text", "favorited", "favoriteCount", "replyToSN",
                         "created", "truncated", "replyToSID", "id", "replyToUID",
                         "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted",
                         "longitude", "latitude") 
   
   stop_words <- read.table("polish_stop_words.txt")[,1]
   
   male_slowa <- tabela %>%
      "$"("text") %>%
      stri_replace_all_regex("@.*?[:/s]", "") %>%
      stri_extract_all_words() %>%
      unlist() %>%
      stri_trans_tolower()
   
   slowa_bez_stop_words <- male_slowa[!(male_slowa %in% stop_words) & 
                                         !stri_detect_regex(male_slowa, "[0-9]")]
   
   if(length(slowa_bez_stop_words)==0) next
   
   najczestsze <- slowa_bez_stop_words %>%
      table() %>%
      sort(decreasing = TRUE) %>%
      head(30)
   
   nazwa <- stri_match_all_regex(one_file_directory, "\\\\twitter\\\\(.*?)_tabela_")[[1]][1,2]
   nazwa_pdf <- stri_paste("indicators//twitter//wykresy//wykresiki_", nazwa, ".pdf")
   
   pdf(nazwa_pdf, title = stri_trans_toupper(nazwa))
   
   
   wordcloud(names(najczestsze),  najczestsze, scale=c(5,0.5), random.order=F, colors="black", 
             min.freq=2)
   
   dev.off()
}











