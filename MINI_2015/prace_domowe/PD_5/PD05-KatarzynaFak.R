# niezbędne biblioteki
library(RMySQL)
library(streamR)
library(dplyr)

# Zamierzam wrzucić wszystkie uzbierane twitty. 

# Łączenie wszystkich, twittów w jedną zbiorczą ramkę danych:
path <- "~/Wybory/dane/twitter/"
file_list <- list.files(path)

for (file in file_list){
      # utworz ramke danych o ile jeszcze nie istnieje
      if (!exists("wszystkie_twitty")){
            wszystkie_twitty <- parseTweets(paste0(path,file), simplify=TRUE,verbose=FALSE)
      }     
      # jeśli zbiorcza ramka już istnieje, dołącz do niej inne pliki
      if (exists("wszystkie_twitty")){
            temp <- parseTweets(paste0(path,file), simplify=TRUE,verbose=FALSE)
            wszystkie_twitty <- rbind(wszystkie_twitty, temp)
            rm(temp)
      }
}
setwd("MINI_2015/prace_domowe/PD_5/")
write.csv(wszystkie_twitty,"wszystkie_twitty.txt")

# łączenie
# haslo <- '64tl1bp6un9sqxp8'
# haslo <- paste0(rev(strsplit(haslo,"")[[1]]),collapse = "")
mpolaczenie = src_mysql(dbname = 'students', host = 'beta.icm.edu.pl',
                        user = 'pbiecek', password = haslo)

# dorzucam swoje:
katarzyna_fak_twitty <- read.csv("wszystkie_twitty.txt")
copy_to(mpolaczenie,katarzyna_fak_twitty,temporary=FALSE)

# czy sie udalo?
dbListTables(mpolaczenie$con)

# juhuuu!

