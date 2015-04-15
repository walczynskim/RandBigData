# consumerKey <- "tG963sbrzaLHYZVFlx4hxNU24"
# consumerSecret <- "ynAYZM2s9bIlJ8a0djCVtQz2YenKBjbUKqQF9RubX6jIjkCenW"
# access_token   <- "3087485255-DQWuT6dXwAKv5KO0imvjViuW9v1VZeEoMvwnkmI"
# access_secret  <- "RVVbb2dlY1NPumnS0A1fBiFLN1GpRYkIXKuBywIo19miv"


consumerKey <- "lKGuvHSRQHnyZcOJh0dvhdC3H"
consumerSecret <- "3i4Eyj15y57C4zQjNHQwcsqTXH4ZR7HpTWKkYvL0SbZrr1yST0"
access_token   <- "1538424414-kM984K5qTERcMm65eyH61t83UuZFhkAR1JCpbps"
access_secret  <- "5Rj0E92flqbstEb84U7pj6yFECAzqvaNwjHMMJiOXCRwZ"

library(twitteR)
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_secret)

#############################################
#nasluch Bronka
bronek_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   ut <- userTimeline("Komorowski", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   #ut<-ut[which(ut[,5]>=od_kiedy),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   
   n<-nrow(ut)
   
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Komorowski"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}


#a<-read.table("D:\\Moje dokumenty\\Emilka\\SMAD\\RAndBigData\\P1\\Tweety\\tweety_kandydatow_2015-03-15.csv",sep=";",h=T)
#nasluch dudy

duda_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   
   ut <- userTimeline("AndrzejDuda", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')   
   ut[,5]<-as.character(ut[,5])
   
   
   n<-nrow(ut)
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Duda"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}

#nasluch Ogorek

ogorek_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   ut <- userTimeline("ogorekmagda", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   n<-nrow(ut)
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Og?rek"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}
#nasluch korwina

korwin_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   
   ut <- userTimeline("korwinmikke", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   
   
   n<-nrow(ut)
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Korwin"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}
#nasluch Palikota

palikot_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   ut <- userTimeline("Palikot_Janusz", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   
   n<-nrow(ut)
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Palikot"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}

#nasluch Jarubasa

jarubas_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   
   ut <- userTimeline("JarubasAdam", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   
   n<-nrow(ut)
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Jarubas"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}

kukiz_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   
   ut <- userTimeline("PrezydentKukiz", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   
   n<-nrow(ut)
   
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Kukiz"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}

grodzka_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   
   ut <- userTimeline("AnnaGrodzka", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   
   n<-nrow(ut)
   
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Grodzka"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}

kowalski_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   
   ut <- userTimeline("M_Kowalski1", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   
   n<-nrow(ut)
   
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Kowalski"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}

nowicka_tweet<-function(od_kiedy=as.POSIXct("2000-01-01"),f){
   
   
   ut <- userTimeline("WandaNowicka", n=100)
   ut<-twListToDF(ut)
   ut<-ut[which(ut[,5]>=od_kiedy & ut[,5]<=od_kiedy+3600*24),]
   ut[,1]<-stri_paste('"',stri_trim_both(stri_replace_all_regex(ut[,1],"(\\n)|(\\t)|(\\r)|(\")"," ")),'"')  
   ut[,5]<-as.character(ut[,5])
   
   
   n<-nrow(ut)
   
   if(n>0){
      ut<-cbind(ut,data.frame(kandydat="Nowicka"))
      ut[,17]<-stri_paste('"',ut[,17],'"') 
      for(i in 1:n){
         
         writeLines(stri_flatten(paste(ut[i,]),collapse=';'),f)
      }
   }
   return(invisible(NULL))
}


#setwd("D:\\Moje dokumenty\\Emilka\\SMAD\\RAndBigData\\P1\\Tweety")

TweetsKandydaci <- function(od_kiedy=as.POSIXct(as.character(Sys.Date()-1))){
   library(streamR)
   library(ROAuth)
   library(twitteR)
   library(stringi)
   
   
   data=as.character(Sys.Date())
   
   fname<-paste0(getwd(),"\\","tweety_kandydatow","_",data, ".csv")
   if (!file.exists(fname)){
      f<-file(fname, open="a")
      #tworze pierwszy wiersz w pliku:
      writeLines(stri_paste('\"text\"', '\"favorited\"', '\"favoriteCount\"','\"replyToSN\"',
                            '\"created\"','\"truncated\"','\"replyToSID\"', '\"id\"',
                            '\"replyToUID\"','\"statusSource\"','\"screenName\"', '\"retweetCount\"',
                            '\"isRetweet\"', '\"retweeted\"','\"longitude\"','\"latitude\"','\"lastname\"',sep = ";"), f)
   }else f<-file(fname, open="a")
   
   
   bronek_tweet(od_kiedy,f)
   duda_tweet(od_kiedy,f)
   ogorek_tweet(od_kiedy,f)
   korwin_tweet(od_kiedy,f)
   palikot_tweet(od_kiedy,f)
   jarubas_tweet(od_kiedy,f)
   kukiz_tweet(od_kiedy,f)
   grodzka_tweet(od_kiedy,f)
   kowalski_tweet(od_kiedy,f)
   nowicka_tweet(od_kiedy,f)
   
   close(f)
   return(invisible(NULL))
}
#twitter_kandydaci()
