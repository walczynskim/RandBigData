install.packages("stringi")
library("stringi")

install.packages("XML")
library("XML")

install.packages("rvest")
library("rvest")

install.packages("dplyr")
library(dplyr)

library(MASS)

# wyszukuje nazwisk aktorow i postaci jakie oni grali
strona <- html("http://www.filmweb.pl/serial/%C5%9Awiat+wed%C5%82ug+Bundych-1987-92457/cast/actors")
tabela_aktor <- readHTMLTable("http://www.filmweb.pl/serial/%C5%9Awiat+wed%C5%82ug+Bundych-1987-92457/cast/actors")
aktor <- as.character(tabela_aktor[[1]]$V2)[-1]
aktor_jako <- as.character(tabela_aktor[[1]]$V3)[-1]
aktorska_tabela <- data.frame(aktor = aktor, postac = aktor_jako)
print(aktorska_tabela)


#wyszukuje odcinki serialu, sezon, nr odcinka i nazwe
h <- html("http://www.filmweb.pl/serial/%C5%9Awiat+wed%C5%82ug+Bundych-1987-92457/episodes/popular")
odcinek <- html_nodes(h, ".normal")
odcinek_tekst <- html_text(odcinek)
nr_odcinka <- character(0)
nazwa_odcinka <- character(0)
for(i in 1:length(odcinek_tekst)){
  if(i%%2 == 0){
    nazwa_odcinka<- c(nazwa_odcinka, odcinek_tekst[i])
  }
  else{
    nr_odcinka <- c(nr_odcinka, odcinek_tekst[i])
  }
}

#wyszukuje oceny i ogladalnosc odcinkow
cos <- html_nodes(h, ".s-13")
tekst <- html_text(cos)
tekst <- tekst[1:264]

liczba <- numeric(0)
ocena <- numeric(0)
for(i in 1 :length(tekst)){
  liczba <- c(liczba, stri_extract_all(tekst[i], regex = "[0-9]+")[[1]][1])
  ocena <- c(ocena, stri_extract_all(tekst[i], regex = "[0-9]+,[0-9]+")[[1]])
}

liczba <- as.numeric(liczba)
for(i in 1:length(ocena)){
  stri_sub(ocena[i], from = 2, to = 2) <- "."
}
ocena <- as.double(ocena)


dane <- data.frame(odcinek = nr_odcinka, nazwa = nazwa_odcinka, ocena = ocena, liczba = liczba)

sezon <- numeric(0)
odcineczek <- numeric(0)
for(i in 1:nrow(dane)){
  sezon <- c(sezon, stri_extract_all(dane$odcinek[i], regex = "[0-9]+")[[1]][1])
  odcineczek <- c(odcineczek, stri_extract_all(dane$odcinek[i], regex = "[0-9]+")[[1]][2])
}

as.numeric(sezon)
dane2 <- data.frame(sezon = as.numeric(sezon), odcineczek = as.numeric(odcineczek), nazwa = nazwa_odcinka, ocena = ocena, liczba = liczba)

#porzadkuje dane wedlug sezonu i odcinka
dane3 <- arrange(dane2, sezon, odcineczek)
lp <- data.frame(lp = 1:nrow(dane3))
dane3 <- cbind(lp, dane3)

#analiza liczby osob oceniajacych odcinki
plot(dane3$liczba, xlab = "lp", ylab = "l. oceniajacych")

# ostatnie 5 odcinkow to odcinki specjalne wyraznie mniej ocenianych
# wyrzuce ich z analizy, dodatkowo wyrzucam pierwszy odcinek

plot(dane3$liczba[2:259], xlab = "lp", ylab = "l. oceniajacych")

# sprubuje dopasowac model liniowy 
model <- lm(liczba ~ lp, data = dane3, subset = c(2:259))
summary(model)

plot(dane3$liczba[2:259], xlab = "lp", ylab = "l. oceniajacych")
abline(coef = summary(model)$coef)
# slabo, zrobimy box'a-cox'a
boxcox(model, lambda = seq(-8, 8, 1/10), plotit = TRUE,
       eps = 1/50, xlab = expression(lambda),
       ylab = "log-Likelihood")

boxcox(model, lambda = seq(-5, 0, 1/10), plotit = TRUE,
       eps = 1/50, xlab = expression(lambda),
       ylab = "log-Likelihood")

BOXCOX <- boxcox(model, lambda = seq(-3, -2, 1/10), plotit = TRUE,
       eps = 1/50, xlab = expression(lambda),
       ylab = "log-Likelihood")
max_log_lik <- which.max(BOXCOX$y)
lambda <- BOXCOX$x[max_log_lik]

model_box <- lm((liczba)^lambda ~ lp, data = dane3, subset = c(2:259))
summary(model_box)

plot(x = dane3$lp[2:259], y = dane3$liczba[2:259]^lambda, xlab = "lp", ylab = "liczba^(-2.66)")
abline(model_box, col = "green")

# z modelu dostajemy, ze liczbe osob oceniajacych dany odcinek podniesiona 
# do potegi -2.66 mozna modelowac funkcja liniowa o wyliczonych 
# wspolczynnikach

# zmienna lp jest istotna w porownaniu do modelu ze srednia




# analizy ocen ogladalnosci

# srednia ocena ogladalnosci
mean(dane3$ocena)
# [1] 8.517992


# srednie ocen ogladalnosci poszczegolnych sezonow
srednie_sezonow <- numeric(11)
names(srednie_sezonow) <- as.character(1:11)
for(i in 1:11){
  srednie_sezonow[i] <- mean(dane3$ocena[which(dane3$sezon == i)])
}

srednie_sezonow
barplot(srednie_sezonow, xlab = "nr.sezonu", ylab = "ocena", main = "Srednie oceny sezonow")

boxplot(dane3$ocena ~ dane3$sezon, xlab = "sezon", ylab = "ocena")

# dopasujmy model liniowy i sprawdzmy czy wraz z kolejnymi sezonami 
# srednia ocena sezonu rosla
s <- 1:11
model_ocena <- lm(srednie_sezonow ~ s)
summary(model_ocena)
plot(x = s, y = srednie_sezonow, xlab = "nr. sezonu", ylab = "srednia ocena")
abline(model_ocena)
# moze model nie jest najlepszy ale wg. niego srednia ocena sezonu lekko wzrasta
# kolejny sezon to wzrost oceny o kolo 0.025
