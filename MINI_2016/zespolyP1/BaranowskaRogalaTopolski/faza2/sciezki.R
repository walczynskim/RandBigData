library(stringi)
library(dplyr)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)


pliki <- list.files(".\\RandBigData\\Projekt 1\\Raporty")

# chodzimy w pętli po miesiącach i po dniach
foreach(i=formatC(1:12, width = 2, flag= "0")) %dopar% {
  library(stringi)
  library(dplyr)
  pliki_mies <- pliki[stri_detect_fixed(pliki, paste0(i, ".txt"))]

  for(j in 1:31){
    dane <- NULL
    
    # dla każdego miesiąca wczytujemy wszystkie ramki, filtrujemy szukany dzien,
    # i laczymy ze sobą z informacja o tym, z ktorej stacji pochodzi dany wiersz
    for(k in pliki_mies){
      tmp <- read.csv(paste0(".\\RandBigData\\Projekt 1\\Raporty\\", k))
      tmp %>% filter(day == j) -> tmp
      if(nrow(tmp)>0) tmp$stacja <- stri_sub(k, from = 11, to = -5)
      dane <- rbind(dane, tmp)
    }
    # wybieramy unikalne id z tego dnia
    unique_id <- unique(dane$visitor_id)
    
    # szukamy sciezek - szukamy kazdego z unikalnych id,
    # wyniki sortujemy po czasie rozpoczecia, i wybieramy tylko niektore kolumny, 
    # zeby mniej miejsca zajmowalo
    # 
    sciezki <- lapply(unique_id, function(x) {
      dane %>%
        filter(visitor_id == x) %>% 
        arrange(as.character(begin_time)) %>%
        select(month, day, begin_time, end_time, visitor_id, stacja)-> dane_un_1
    })
    day <- formatC(j, width = 2, flag= "0")
    sciezki_df <- do.call(rbind, sciezki)
    write.csv(sciezki_df, file= file.path("RandBigData","Projekt 1","sciezki",paste0(day, "_", i, ".txt")))
  }
}
stopCluster(cl)

#-------------------#
#Zmieniam nazwy stacji i czyszcze folder 'sciezki' z pustych plikow
#!ZROBIC W GLOWNEJ PETLI OD RAZU DLA INNYCH LAT

setwd("/home/zosia/studia/RandBigData/Projekt_I/ScrappingTextMining/sciezki")
filenames <- list.files()

#usuwam puste pliki ->moze lepiej ich wcale nie tworzyc
info = file.info(filenames)
file.remove(filenames[info$size == 3])

filenames <- list.files()
filename = filenames[1]

#poprawiam nazwy stacji
for(filename in filenames){
  data <- read.table(filename, sep = ",", header = TRUE)[,-1]
  names(data)[6] <- "exhibit"
  data$exhibit <- stri_sub(data$exhibit, from = 1, to = -4)
  write.csv(data, file.path("..", 'sciezki_dane2013',filename))
}