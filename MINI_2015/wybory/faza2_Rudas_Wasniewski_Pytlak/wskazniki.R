library(stringi)
library(dplyr)
library(tidyr)

kandydat <- c(
   "Komorowski\\p{L}*",
   "Marian(\\p{L})* Kowalsk(\\p{L})*",
   "(Dud(\\p{L})*)",
   "Paliko(\\p{L})*",
   "Jarubas(\\p{L})*",
   "Ogórek|OgÃ³rek",
   "Korwin(\\p{L})*",
   "Wilk(\\p{L})*",
   "Braun(\\p{L})*",
   "Kukiz(\\p{L})*"
)
nazwiska <- c("komorowski", "kowalski", "duda", " palikot", "jarubas", "ogórek", 
              "korwin","wilk","braun","kukiz")
names(kandydat) <- nazwiska

# sciezka do katalogu z danymi:
sciezka <- "C:/Users/Miko³aj/Documents/SMAD/R and Big Data/wybory/d"


# Funkcja zwracaj¹ca liczbe wystapienia nazwiska kandydata na dzien
# z podzialem na zrodla artykulow 
# co-nazwa kolumny np. tytul, tagi, tresc
ile.dziennie <- function(sciezka, co = "tytul", po.zrodle=FALSE){
   #Wybieramy najbardziej aktualny plik
   lista <- list.files(sciezka)
   aktualny <- lista[length(lista)]
   
   #Sciezka dostepu do aktualnego
   sciezka <- file.path(sciezka, aktualny)
   
   dane <- read.table(sciezka, stringsAsFactors=FALSE, h=TRUE)
   
   czy_nazwisko<-sapply(kandydat, function(x){
      stri_count_regex(dane[, co], x)
   })
   d2<-cbind(dane[,2:3], czy_nazwisko)
   d2<-separate(d2, data, c("dzien", "godzina"), " ")
   # niektore daty maja format rok-miesiac-dzien, sprowadzam wszystkie
   # daty do formatu dd-mm-rrrr:
   d2$dzien <- stri_replace_all_regex(d2$dzien,
                                      "([0-9]{4})-([0-9]{2})-([0-9]{2})", 
                                      "$3-$2-$1")

   if (po.zrodle){
      d2<-d2 %>% group_by(dzien, zrodlo)
   } else {
      d2<-d2 %>% group_by(dzien)
   }
    d2 %>% 
      summarise_each(funs(sum), -zrodlo, -dzien, -godzina)
}
i<-ile.dziennie(sciezka)
ile.dziennie(sciezka, "tresc", po.zrodle = FALSE)
ile.dziennie(sciezka, "tagi")
write.table(i,"w_tytule_dziennie.txt")


# funkcaj zliczaj¹ca liczbe wystapien nazwiska kandydata w artykule
# parametr tylko.wstep=TRUE - wystapienie nazwiska kandydata tylo z 
# pierwszego akapitu 
nazwiska.w.tresci<-function(sciezka, tylko.wstep=FALSE){
   #Wybieramy najbardziej aktualny plik
   lista <- list.files(sciezka)
   aktualny <- lista[length(lista)]
   #Sciezka dostepu do aktualnego
   sciezka <- file.path(sciezka, aktualny)
 
   dane<-read.table(sciezka, stringsAsFactors=FALSE, h=TRUE)
   if (tylko.wstep) {
      tresc <- stri_extract_first_regex(dane$tresc, "[^\\n|]+")
   } else {
      tresc <- dane$tresc
   }
   czy_nazwisko <- sapply(kandydat, function(x){
      stri_count_regex(tresc, x)
   })
   cbind(dane[, 1:4], czy_nazwisko) 
}
d<-nazwiska.w.tresci(sciezka, tylko.wstep=FALSE)
head(d)
d<-nazwiska.w.tresci(sciezka, tylko.wstep=TRUE)
tail(d)
write.table(d,"nazwiska_w_tresci.txt")



# Funkcja zwracajaca liczbe wystapien nazwiska w tytule 
# z podzialem na pore dnia. 
# po.portalach- czy z podzielm na portale, z ktorych pobieramy dane 
pora.dnia <- function(sciezka, co="tytul", po.portalach=TRUE){
   #Wybieramy najbardziej aktualny plik
   lista <- list.files(sciezka)
   aktualny <- lista[length(lista)]
   #Sciezka dostepu do aktualnego
   sciezka <- file.path(sciezka, aktualny)
   
   dane<-read.table(sciezka,stringsAsFactors=FALSE,h=TRUE)
   czy_nazwisko <- sapply(kandydat, function(x){
      stri_count_regex(dane[, co],x)
   })
   dane <- cbind(dane, czy_nazwisko)
   dane <- separate(dane, data, c("dzien", "godzina"), " ")
   
   dane$dzien <- stri_replace_all_regex(dane$dzien,
                                      "([0-9]{4})-([0-9]{2})-([0-9]{2})", 
                                      "$3-$2-$1")
   
   godzina <- stri_extract_first_regex(dane$godzina, "[0-9]{2}") %>% as.numeric()
   
   pora<-character(nrow(dane))
   pora[godzina %in% 0:5] <- "w nocy"
   pora[godzina %in% 06:11] <- "rano"
   pora[godzina %in% 12:17] <- "w ciagu dnia"
   pora[godzina %in% 18:23] <- "wieczorem"
   
   dane$pora<-pora
   if (po.portalach){
      d <- dane[, c(2:3,9:19)] %>% group_by(zrodlo,pora)
   } else {
      d <- dane[, c(2:3,9:19)] %>% group_by(pora)
   } 
   d %>%
      summarise_each(funs(sum), -zrodlo, -dzien, -pora)
}

pora.dnia(sciezka)
pora.dnia(sciezka, po.portalach=FALSE)
pora.dnia(sciezka, co="tresc", po.portalach=FALSE)
pora.dnia(sciezka, co="tresc")



# funkcja zwracaj¹ca ramke danych z danymi o artykulach i iformacj¹ 
# czy nazwsiko kandydata wystepuje w tytule
kto.w.tyt <- function(sciezka){
   #Wybieramy najbardziej aktualny plik
   lista <- list.files(sciezka)
   aktualny <- lista[length(lista)]
   #Sciezka dostepu do aktualnego
   sciezka <- file.path(sciezka,aktualny)
   
   dane<-read.table(sciezka,stringsAsFactors=FALSE,h=TRUE)
   czy_nazwisko<-sapply(kandydat,function(x){
      stri_count_regex(dane$tytul,x)
   })
   cbind(dane, czy_nazwisko)
}

# funkcja agregujaca liczbe komentarzy dla kazdego kandydata
# po,zrodlach- czy z podzialem na zrodla artykulow
# fun-funkcja agregujaca
komentarze <- function(sciezka, fun=mean, po.zrodle = TRUE){
   d<-kto.w.tyt(sciezka)

   if (po.zrodle) {
      zrodla <- unique(d$zrodlo)
      d3<-data.frame()
      for (i in 1:length(zrodla)){
         d2 <- d[d$zrodlo == zrodla[i],]
         f <- sapply(names(kandydat),function(x){
            j <- which(d2[,x]>0)
            fun(d[j,"liczba.komentarzy"])
         })
         d3 <- rbind(d3,f)
      }
      names(d3) <- names(kandydat)
      d3 <- cbind(zrodla,d3)
   } else {
      d3 <- sapply(names(kandydat), function(x){
         j <- which(d[, x]>0)
         fun(d[j, "liczba.komentarzy"])
      })
   }
   d3
}

komentarze(sciezka ,mean)
komentarze(sciezka, mean, po.zrodle=FALSE)
komentarze(sciezka, median)
komentarze(sciezka, median, po.zrodle=FALSE)
komentarze(sciezka, length)  #liczba artykulow z komentarzami
                            # dla kazdego kandydata
komentarze(sciezka, sum)

