# wczytuje zebrane, zapisane i wyczyszczone dane
dane_tweet_nasluch <- read.csv(file = "C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane_tweet_nasluch.csv", 
                               row.names = 1)
dane_tweet_kandydat <- read.csv(file = "C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane_tweet_kandydat.csv", 
                                row.names = 1)
dane_portale <- read.csv(file = "C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane_portale.csv", 
                         row.names = 1)

# lacze dane w jedna ramke danych
dane <- rbind(dane_tweet_nasluch[,1:4], dane_tweet_kandydat[,1:4],
              dane_portale[,1:4])

# zamieniam tekst na male litery
dane$tekst <- stri_trans_tolower(dane$tekst)

#ustalam o jakim kandydacie jest dany tekst (moze byc o kilku)
komorowski <- as.numeric(stri_detect_fixed(dane$tekst, "komorowsk"))
duda <- as.numeric(stri_detect_fixed(dane$tekst, "dud"))
jarubas <- as.numeric(stri_detect_fixed(dane$tekst, "jarubas"))
ogorek1 <- as.numeric(stri_detect_fixed(dane$tekst, "ogorek"))
ogorek2 <- as.numeric(stri_detect_fixed(dane$tekst, "ogórek"))
ogorek <- ifelse(ogorek1+ogorek2 == 0, 0, 1)
palikot <- as.numeric(stri_detect_fixed(dane$tekst, "paliko"))
korwin <- as.numeric(stri_detect_fixed(dane$tekst, "korwin"))
kukiz <- as.numeric(stri_detect_fixed(dane$tekst, "kukiz"))

dane <- cbind(dane, komorowski = komorowski, duda = duda, 
              jarubas = jarubas, ogorek = ogorek, palikot = palikot, 
              korwin = korwin, kukiz = kukiz)

#usuwam teksty, ktore nie tycza sie wymienionych kandydatow
zostaw <- ifelse(komorowski + duda + jarubas + ogorek + palikot + korwin + kukiz == 0, FALSE, TRUE)
dane <- dane[zostaw,]

#########  pozytywne i negatywne slowa - github Biecek
######### tlumacze tekst na angielski
######### http://www.onlinedoctranslator.com/translator.html
# wczytuje slowniki slow pozytywnych i negatywnych

pozytyw <- readLines("C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\pozytyw.en.pl.txt", encoding = "UTF-8")
pozytyw <- stri_replace_all_fixed(pozytyw, pattern = "\t", replacement = "")
pozytyw <- stri_trans_tolower(pozytyw)

negatyw <- readLines("C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\negatyw.en.pl.txt", encoding = "UTF-8")
negatyw <- stri_replace_all_fixed(negatyw, pattern = "\t", replacement = "")
negatyw <- stri_trans_tolower(negatyw)


########   'nasze' pozytywne i negatywne gloski
########   http://home.agh.edu.pl/~horzyk/lectures/pn/ahdydpnmagiaslow.php

pozytyw_gloski <- readLines("C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\poz_nasze.txt")
negatyw_gloski <- readLines("C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\neg_nasze.txt")


# funkcja score zwraca dla tekstu roznice miedzy liczba slow pozytywnych 
# a liczba slow negatywnych pochodzacych z zadanych slownikow
score <- function(tekst, poz = pozytyw, neg = negatyw){
  score <- numeric(length(tekst))
  for(i in 1:length(tekst)){
    plus <- sum(stri_count_fixed(tekst[i], poz))
    minus <- sum(stri_count_fixed(tekst[i], neg))
    score[i] <- plus - minus
  }
  return(score)
}

scores1 <- score(dane$tekst)
scores2 <- score(dane$tekst, poz = pozytyw_gloski, neg = negatyw_gloski)

dane <- cbind(dane, score1 = scores1, score2 = scores2)


write.csv(dane, "C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane.csv")




