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
nazwiska <- c("komorowski","kowalski","duda","palikot","jarubas","ogórek","korwin",
              "wilk","braun","kukiz")
names(kandydat) <- nazwiska




# sciezki do plikow
sciezka.na.temat<-"C:\\Users\\Miko³aj\\Documents\\SMAD\\R and Big Data\\wybory\\Dane_glowne\\NaTematGlowna"
sciezka.onet.glowna<-"C:\\Users\\Miko³aj\\Documents\\SMAD\\R and Big Data\\wybory\\Dane_glowne\\OnetGlowna"
sciezka.tvn24 <- "C:\\Users\\Miko³aj\\Documents\\SMAD\\R and Big Data\\wybory\\Dane_glowne\\tvn24"

# funkcja do wczytywania danych z na temat strony glownej
dane.na.temat.glowna <- function(sciezka){
   w<-list.files(sciezka,
                 recursive=FALSE)
   w<-w[stri_detect_regex(w,"\\.txt")]
   sciezka2<-file.path(sciezka,w)
   
   dane<-read.table(sciezka2[1], stringsAsFactors=FALSE, sep=",", h=TRUE)
   for (i in 2:length(sciezka2)){
      dane<-rbind(dane,read.table(sciezka2[i], stringsAsFactors=FALSE, sep=",", h=TRUE))
   }
   dane <- unique(dane)
   dane
}
dane<-dane.na.temat.glowna(sciezka.na.temat)




# wczytanie plikow  z onet stony glownej i tvn24 stony glownej 
dane.glowna<-function(sciezka, recursive=TRUE){
   w<-list.files(sciezka,
                 recursive=TRUE)
   w<-w[stri_detect_regex(w,"\\.txt")]
   sciezka2<-file.path(sciezka,w)
   
   dane<-read.table(sciezka2[1], stringsAsFactors=FALSE, sep=",", h=TRUE)
   dane$data<-stri_extract_last_regex(sciezka2[1],"[0-9-]+@[0-9-]+")
   for (i in 2:length(sciezka2)){
      dane.temp <- read.table(sciezka2[i], stringsAsFactors=FALSE, sep=",", h=TRUE)
      dane.temp$data <- stri_extract_last_regex(sciezka2[i],"[0-9-]+@[0-9-]+")
      dane<-rbind(dane, dane.temp)
   }
   dane <-dane[!duplicated(dane[, 1:4]),]
   dane
}

dane<-dane.glowna(sciezka.onet.glowna)
dane<-dane.glowna(sciezka.tvn24)

# funkcja liczaca ile razy dany kandydat pojawil sie na w tytule na 
# stronie glownej z podzilem na dni 
kandydat.dziennie<-function(sciezka, na.temat=FALSE){
   if (na.temat) {
      dane <- dane.na.temat.glowna(sciezka)
   } else {
      dane <- dane.glowna(sciezka.tvn24)
   }
   czy_nazwisko <- sapply(kandydat, function(x){
      stri_count_regex(dane$tytul, x)
   })
   d <- select(dane,data)
   d2 <- cbind(d, czy_nazwisko)
   d2 <- separate(d2, data, c("dzien", "godzina"), "@|[ ]")
   d2 %>% group_by(dzien) %>% 
      summarise_each(funs(sum), -dzien, -godzina)
}
d<-kandydat.dziennie(sciezka.tvn24)
kandydat.dziennie(sciezka.onet.glowna)
kandydat.dziennie(sciezka.na.temat, na.temat = TRUE)


# miary widocznosci kandydata dla TVN24
miary.tvn <- function(sciezka, dzien=TRUE, czcionka=FALSE, status=FALSE){
   
   d<-dane.glowna(sciezka)
   czy_nazwisko<-sapply(kandydat,function(x){
      stri_count_regex(d$tytul,x)
   })
   d2<-cbind(d, czy_nazwisko)
   d2<-separate(d2, data, c("dzien", "godzina"), "@")
   
   if (dzien){
      d2<-d2 %>% group_by(dzien) 
   }
   if (czcionka){
      d2<-d2 %>% group_by(czcionka) 
   }
   if (status){
      d2<-d2 %>% group_by(status) 
   }
   if (dzien & status){
      d2<-d2 %>% group_by(dzien,status) 
   }
   d2 %>% 
      summarise_each(funs(sum),-tytul,-dzien,-godzina,-artykul,-status,-czcionka)
}
miary.tvn(sciezka.tvn24)  # nazwisko w tytule dziennie 
miary.tvn(sciezka.tvn24, dzien=FALSE, czcionka=TRUE)  # czcionka a nazwisko kandydata
miary.tvn(sciezka.tvn24, dzien=FALSE, status=TRUE)  # status a nazwisko kandydata
miary.tvn(sciezka.tvn24, dzien=TRUE, status=TRUE)  # status a nazwisko kandydata
                                                # z podzia³em na dni


#miary dla onetu strony glownej 
miary.onet <- function(sciezka,dzien=TRUE){
   d<-dane.glowna(sciezka.onet.glowna)
   czy_nazwisko<-sapply(kandydat,function(x){
      stri_count_regex(d$tytul,x)
   })
   d2<-cbind(d, czy_nazwisko)
   d2<-separate(d2, data, c("dzien", "godzina"), "@")
   if (dzien){
      d2<-d2 %>% group_by(dzien,waznosc)
   } else {
      d2<-d2 %>% group_by(waznosc)
   }
    d2 %>% 
      summarise_each(funs(sum),-tytul,-dzien,-godzina,-tresc,-url,-waznosc)
}

miary.onet(sciezka.onet.glowna)  # waznosc z podza³em na dni
miary.onet(sciezka.onet.glowna, dzien=FALSE) # waznosc ogolem


