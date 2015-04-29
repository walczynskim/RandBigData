library(dplyr)
library(stringi)

katalog <- "C:\\Dane\\Pawel_2\\PW\\R_Big_Data\\Projekt1\\tweety\\oficjalne_tweety"

kandydaci <- c("Adam_Jarubas", "Andrzej_Duda", "Bronislaw_Komorowski", 
               "Janusz_Korwin-Mikke", "Janusz_Palikot", "Magdalena_Ogorek", 
               "Pawel_Kukiz")

oficTweety <- list() 

#zamieniam pliki tekstowe z oficjalnymi tweetami poszczegolnych kandydatow 
#na ramki danych
for(i in seq_along(kandydaci)){
   oficTweety[[i]] <- read.table(stri_paste(katalog, "\\", kandydaci[i], ".txt"), 
                                 header = TRUE, sep = "", row.names = NULL)
   names(oficTweety[[i]]) <- c("tekst", "kandydat", "dzien", "godzina")    
}

#lacze wszystkie ramki danych w jedna ramke
Pawel_Pytlak_tweety <- do.call("rbind", oficTweety)

#inicjuje polaczenie z baza danych
polaczenie <- src_mysql(dbname = "students", host = "beta.icm.edu.pl", 
                        user = "pbiecek", password = haslo)

#zapisuje tabele w bazie danych
copy_to(polaczenie, Pawel_Pytlak_tweety, temporary = FALSE)

