## WCZYTANIE MACIERZY Z PLIKU:
load('C:\\Users\\grabarze\\Desktop\\moviesInfoDist.rda')

# WCZYTANIE BAZY DANYCH ABY WYCIAGNAC NAZWY FILMOW
##
library(dplyr)
library(stringi)
haslo <- '64tl1bp6un9sqxp8' %>%
  stri_reverse()
library(RMySQL)
sterownik <- MySQL()
mpolaczenie = dbConnect(sterownik, 
                        user='pbiecek', password=haslo, dbname='students', 
                        host='beta.icm.edu.pl')


dbListTables(mpolaczenie)
dbGetQuery(mpolaczenie, "SELECT count(*) 
           FROM Grabarz_Kosinski_Wasniewski")
# pobranie wszystkich filmow
movies <- dbGetQuery(mpolaczenie, "SELECT * 
                     FROM Grabarz_Kosinski_Wasniewski")
##

## INSTALACJA PAKIETU DO MALOWANIA WYKRESOW:
library(devtools)
#install_github('sinhrks/ggfortify')
library(ggfortify)




## WYKRES 1, skaloanie metrycznie wielowymiarowe
moviesInfoDist_cmdscale <- as.matrix(moviesInfoDist)[1:100,1:100]
rownames(moviesInfoDist_cmdscale) <- movies$title[1:100]
colnames(moviesInfoDist_cmdscale) <- movies$title[1:100]
mds1 <- cmdscale(moviesInfoDist_cmdscale, eig = TRUE)
# plot(mds1, type = 'n')
# text(mds1, rownames(mds1), cex = 0.6)
autoplot(mds1, label = TRUE)





## WYKRES 2, heatmapa na macierzy podobienstwa

moviesInfoDist_heatmap <- as.matrix(moviesInfoDist)[1:100,1:100]
rownames(moviesInfoDist_heatmap) <- movies$title[1:100]
colnames(moviesInfoDist_heatmap) <- movies$title[1:100]
autoplot(moviesInfoDist_heatmap)

## WYKRES 3, skalowanie niemetryczne wielwymiarowe
#isoMDS
library(MASS)
moviesInfoDist_isoMDS <- as.matrix(moviesInfoDist)[1:100,1:100]
mds2 <- isoMDS(moviesInfoDist_isoMDS)# albo sammon
# autoplot(mds2 , colour = 'orange', size = 4, label.colour = 'blue')
# autoplot(sammon(eurodist), shape = FALSE, label.colour = 'blue')
# tu mi bledy wyskakuja bo 
# Error in sammon(moviesInfoDist_isoMDS) : 
# zero or negative distance between objects 1 and 33
