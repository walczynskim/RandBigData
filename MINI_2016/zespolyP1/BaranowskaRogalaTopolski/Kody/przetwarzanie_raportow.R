### PRZETWORZENIE I ZAPIS DANYCH (OBLICZENIE CZASU UZYTKOWANIA, DODANIE KOLUMNY W FORMACIE DATY) - 58 PLIKOW ######

library(stringi)
library(dplyr)

czas <- Sys.time()
# by dobrze odczytywalo skroty Feb, Mar jako miesiace, ustawiamy local time
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
# Sys.setlocale("LC_TIME", lct) by wrocic do swojego poprzedniego local time

#funkcja pomocnicza (wektorowa)
#bierzemy nasze czasy bedace characterami w formie "%H:%M:%S" i liczymy roznice miedzy nimi w sekundach
roznica_czasu <- function(czas_potem, czas_przedtem, units="secs", format="%H:%M:%S"){
   
   # jesli roznica czasu po a czasu przed jest ujemna, to minelismy polnoc po drodze
   roznica <- as.numeric(difftime(strptime(czas_potem,format = format ), 
                                          strptime(czas_przedtem,format = format ), 
                                          units=units))
   roznica[roznica <0] <- as.numeric(difftime(strptime(paste0("2013-01-02 ",czas_potem[roznica <0]),
                                                      format = paste0("%Y-%m-%d ",format) ), 
                                             strptime(paste0("2013-01-01 ",czas_przedtem[roznica <0]),
                                                      format = paste0("%Y-%m-%d ",format) ), 
                                             units=units))
   
   roznica
}

folder_do_zapisu_przetworzonych <- file.path("C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt 1/ScrappingTextMining-master/Przetworzone")

if(!dir.exists(folder_do_zapisu_przetworzonych)){
   dir.create(folder_do_zapisu_przetworzonych)
}

sciezka_raporty <- file.path("C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt 1/ScrappingTextMining-master/Raporty")

raporty <- list.files(sciezka_raporty, full.names = T)

#lista urzadzen
lista_urzadzen <- unique(unlist(lapply(stri_match_all_regex(raporty, "_(.+)_"), function(x){x[2]})))

rok <- "2013"


for(urzadzenie in lista_urzadzen){
   
   #bedziemy przetwarzac w tej iteracji tylko pliki dla danego urzadzenia
   raporty_urz <- raporty[stri_detect_fixed(raporty,urzadzenie)]
   
   zbiory <- lapply(raporty_urz, function(raport){
      
      print(raport) # do kontroli postepow
      dane <- read.csv(raport, header = T)
      
      # dodaje do danych info o: czas uzytkowania - diff_time, i date w formie characteru: 2013-11-02 
      # (nie wstawiam obiektu typu data do data.frame!)
      dane %>% filter(is_active == TRUE) %>% mutate(diff_time = roznica_czasu(end_time, begin_time),
                                                    date = as.character(as.Date(paste(rok, month, day), format="%Y %b %e")) ) %>% 
         select(date, begin_time, visitor_id, end_time, diff_time) -> dane
      
      
   }
   )
   
   sciezka_zapisu_danych_1 <- file.path(folder_do_zapisu_przetworzonych,
                                        paste0("przetworzone_",urzadzenie,".csv"))
   
   #laczymy w jedna ramke danych i zapisujemy
   dane_calosc_diff <- do.call(args = zbiory, what=rbind)
   write.csv(dane_calosc_diff, file= sciezka_zapisu_danych_1, row.names = F)
   
}

print(Sys.time() - czas) # 3 minuty u mnie




# to samo ale zapisujemy do jednego pliku, do glownego folderu (144 MB)

### PRZETWORZENIE I ZAPIS DANYCH (OBLICZENIE CZASU UZYTKOWANIA, DODANIE KOLUMNY W FORMACIE DATY) - 1 PLIK ######

library(stringi)
library(dplyr)

czas <- Sys.time()
# by dobrze odczytywalo skroty Feb, MAr jako miesiace, ustawiamy local time
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
# Sys.setlocale("LC_TIME", lct) by wrocic do swojego poprzedniego local time


sciezka_raporty <- file.path("C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt 1/ScrappingTextMining-master/Raporty")

sciezka_zapisu_danych_1 <- file.path("C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt 1/ScrappingTextMining-master",
                                     "dane_calosc_diff.csv")

raporty <- list.files(sciezka_raporty, full.names = T)

rok <- "2013"

#funkcja pomocnicza (wektorowa)
#bierzemy nasze czasy bedace characterami w formie "%H:%M:%S" i liczymy roznice miedzy nimi w sekundach
roznica_czasu <- function(czas_potem, czas_przedtem, units="secs", format="%H:%M:%S"){
   
   # jesli roznica czasu po a czasu przed jest ujemna, to minelismy polnoc po drodze
   roznica <- as.numeric(difftime(strptime(czas_potem,format = format ), 
                                  strptime(czas_przedtem,format = format ), 
                                  units=units))
   roznica[roznica <0] <- as.numeric(difftime(strptime(paste0("2013-01-02 ",czas_potem[roznica <0]),
                                                       format = paste0("%Y-%m-%d ",format) ), 
                                              strptime(paste0("2013-01-01 ",czas_przedtem[roznica <0]),
                                                       format = paste0("%Y-%m-%d ",format) ), 
                                              units=units))
   
   roznica
}

zbiory <- lapply(raporty, function(raport){
   
   print(raport) # do kontroli postepow
   dane <- read.csv(raport, header = T)
   urzadzenie <- stri_match_first_regex(raport, "_(.+)_")[2]
   
   # dodaje do danych info o maszynie - device, czas uzytkowania - diff_time, i date w formie characteru: 2013-11-02 
   # (nie wstawiam obiektu typu data do data.frame!)
   dane %>% filter(is_active == TRUE) %>% mutate(device = urzadzenie, 
                                                 diff_time = roznica_czasu(end_time, begin_time),
                                                 date = as.character(as.Date(paste(rok, month, day), format="%Y %b %e")) ) %>% 
      select(device, date, begin_time, visitor_id, end_time, diff_time) -> dane
}
)

#laczymy w jedna ramke danych i zapisujemy
dane_calosc_diff <- do.call(args = zbiory, what=rbind)
write.csv(dane_calosc_diff, file= sciezka_zapisu_danych_1, row.names = F)
print(Sys.time() - czas) # u mnie to zajelo 4 minuty, wiec tego nie wsadzalam w parallel
