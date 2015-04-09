library("dplyr")
library("tidyr")

###############         ILOSC INFO

# 274 MB, 8400 plikow  ~ 32 000 twittow, artykulow

# wczytuje wyczyszczone i zebrane dane razem z sentymentem
dane <- read.csv("C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane.csv", 
                 row.names = 1)

####################### WSKAZNIKI DLA KANDYDATOW  

# analiza sentymentu oparta bedzie o dwa rozne slowniki:
#   - slow lub wyrazen pozytywnych i negatywnych
#   - podrupy literowe o wydzwieku pozytywnym i negatywnym
#       http://home.agh.edu.pl/~horzyk/lectures/pn/ahdydpnmagiaslow.php


# 1. p-stwo tekstu o danym kandydacie ('niewazne jak mowia, wazne ze mowia')
#    - w calosci (wszystkie portale)
#    - w calosci osobno dla roznych portali
#    - w czasie (wszystkie portale)
#    - w czasie dla roznych portali

wskaznik1 <- function(wektor_nazwisko){
  n <- length(wektor_nazwisko)
  procent <- floor(sum(wektor_nazwisko)/n*10000)/100
  return(procent)
}

# dla wszystkich portali
wskaznik1(dane$komorowski)

# grupujac po portalach
dane %>% group_by(portal) %>% 
  summarise(procent_obecnosci = wskaznik1(komorowski))

# grupujac po dacie (czyli w czasie)
dane %>% group_by(data) %>% summarise(procent_obecnosci = wskaznik1(komorowski))

# grupujac po dacie i portalach
dane %>% group_by(data, portal) %>% summarise(procent_obecnosci = wskaznik1(komorowski))


# 2. p-stwo tekstu o wydzwieku pozytywnym (negatywnym, neutralnym) o danym kandydacie
#    - w calosci (wszystkie portale)
#    - w calosci osobno dla roznych portali
#    - w czasie (wszystkie portale)
#    - w czasie dla roznych portali

wskaznik2_poz <- function(wektor_nazwisko, wektor_sentyment){
  p_m <- ifelse(wektor_sentyment > 0, 1, 0)
  n <- sum(wektor_nazwisko)
  procent <- floor(sum(wektor_nazwisko*p_m*10000)/n)/100
  return(procent)
}
wskaznik2_neg <- function(wektor_nazwisko, wektor_sentyment){
  p_n <- ifelse(wektor_sentyment < 0, 1, 0)
  n <- sum(wektor_nazwisko)
  procent <- floor(sum(wektor_nazwisko*p_n)/n*10000)/100
  return(procent)
}
wskaznik2_neu <- function(wektor_nazwisko, wektor_sentyment){
  p_n <- ifelse(wektor_sentyment == 0, 1, 0)
  n <- sum(wektor_nazwisko)
  procent <- floor(sum(wektor_nazwisko*p_n)/n*10000)/100
  return(procent)
}

# dla wszystkich portali
wskaznik2_poz(dane$duda, dane$score1)
wskaznik2_neg(dane$duda, dane$score1)
wskaznik2_neu(dane$duda, dane$score1)

# grupujac po portalach
dane %>% group_by(portal) %>% 
  summarise(procent_obecnosci_poz = wskaznik2_poz(duda, wektor_sentyment = score1))

dane %>% group_by(portal) %>% 
  summarise(procent_obecnosci_neg = wskaznik2_neg(duda, wektor_sentyment = score1))

dane %>% group_by(portal) %>% 
  summarise(procent_obecnosci_neu = wskaznik2_neu(duda, wektor_sentyment = score1))

# podobnie wskazniki w czasie



# 3. stosunek liczby artykulow pozytywnych do negatywnych o danym kandydacie
#    - w calosci (wszystkie portale)
#    - w calosci osobno dla roznych portali
#    - w czasie (wszystkie portale)
#    - w czasie dla roznych portali

wskaznik3 <- function(wektor_nazwisko, wektor_sentyment){
  return(wskaznik2_poz(wektor_nazwisko, wektor_sentyment)/wskaznik2_neg(wektor_nazwisko, wektor_sentyment))
}

# dla wszystkich portali
wskaznik3(dane$jarubas, dane$score1)

# grupujac po portalach
dane %>% group_by(portal) %>% 
  summarise(poz_do_neg = wskaznik3(duda, wektor_sentyment = score1))

# podobnie reszta

# 4. szansa wystapienia tekstu pozytywnego (negatywnego, neutralnego) dla danego kandydata
#    - w calosci (wszystkie portale)
#    - w calosci osobno dla roznych portali
#    - w czasie (wszystkie portale)
#    - w czasie dla roznych portali

wskaznik4_poz <- function(wektor_nazwisko, wektor_sentyment){
  p <- wskaznik2_poz(wektor_nazwisko, wektor_sentyment)/100
  szansa <- p/(1-p)
  return(szansa)
}
wskaznik4_neg <- function(wektor_nazwisko, wektor_sentyment){
  p <- wskaznik2_neg(wektor_nazwisko, wektor_sentyment)/100
  szansa <- p/(1-p)
  return(szansa)
}
wskaznik4_neu <- function(wektor_nazwisko, wektor_sentyment){
  p <- wskaznik2_neu(wektor_nazwisko, wektor_sentyment)/100
  szansa <- p/(1-p)
  return(szansa)
}


# dla wszystkich portali
wskaznik4_poz(dane$ogorek, dane$score1)
wskaznik4_neg(dane$ogorek, dane$score1)
wskaznik4_neu(dane$ogorek, dane$score1)

# grupujac po portalach
dane %>% group_by(portal) %>% 
  summarise(szansa_poz = wskaznik4_poz(ogorek, wektor_sentyment = score1))
dane %>% group_by(portal) %>% 
  summarise(szansa_poz = wskaznik4_neg(ogorek, wektor_sentyment = score1))
dane %>% group_by(portal) %>% 
  summarise(szansa_poz = wskaznik4_neu(ogorek, wektor_sentyment = score1))

# podobnie reszta


# 5. liczba retwitniec pozytywnych - liczba retwitniec negatywnych
#  dla twittera (z nasluchu) mamy informacje o liczbie udostepnien, 
#  wykorzystamy to do 'wagowania' twitow

# parametr co odpowiada liczbe 'retwitniec' lub liczbie 'polubien'

wskaznik5 <- function(wektor_nazwisko, wektor_sentyment, co){
  p_m <- sign(wektor_sentyment)
  wsk <-sum(wektor_nazwisko*p_m*co)
  return(wsk)
}

# dla ponizszych wskaznikow mozna zastosowac metody szeregow czasowych oraz 
# prosta MNK (jezeli dane beda sensowne, beda spelnialy jakies zalozenia) do 
# sprawdzenia czy wystepuje jakis trend w podanych wskaznikach, wspolczynnik 
# nachylenia prostej jako sila trendu (i kierunek) - ale to przy trzeciej czesci





#   AUTOMATYZACJA

wektor_nazwisk <- c("komorowski", "duda", "jarubas", "ogorek", "palikot", "korwin", "kukiz")

for(i in 1:length(wektor_nazwisk)){
  wsk <- wskaznik3(wektor_nazwisk[i], wektor_sentyment = score1)
}




