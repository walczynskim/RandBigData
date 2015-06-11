i <- 350



n<-5



returnFamiliar <- function( matrixInput, n, i){

# mamy dla niego najbardziej podobne filmy
which( matrixInput[i,] > 0 ) -> numerki_podobnych
closest <- order(matrixInput[i,numerki_podobnych])[1:n]
numerki_podobnych[closest] -> n_najblizszych_do_i

matrix(0,n,n) -> pomocnicza
kk<-0
for(j in n_najblizszych_do_i){
   kk <- kk+1
  which( moviesInfo[j,] > 0 ) -> numerki_podobnych
  
closest <- order(matrixInput[j,numerki_podobnych])[1:n]
numerki_podobnych[closest] -> n_najblizszych_do_j
pomocnicza[kk,] <- n_najblizszych_do_j

}


matrixInput[ c(i,n_najblizszych_do_i, unique(as.vector(pomocnicza))),
            c(i,n_najblizszych_do_i, unique(as.vector(pomocnicza)))] -> podobne_do_i

return(podobne_do_i)
}

returnFamiliar(moviesInfo,n,i) -> podobne_do_i 
mm <- as.matrix(podobne_do_i)

library(reshape)
#zamieniam na postac kolumnowa 
mmr <-  melt(mm)[melt(upper.tri(mm))$value,]
#sorutje po X1 X2
mmr <- mmr[order(mmr[,1],mmr[,2]),]
mmr2 <- mmr[mmr[, 3]>0,] 
nazwy <- unique(sort(c(as.character(mmr2[,1]),as.character(mmr2[,2]))))
# numery wezlow musza byc od 0(zamieniam to tak)
mmr2[, 1] <- as.integer(as.factor(mmr2[, 1]))-1
mmr2[, 2] <- as.integer(as.factor(mmr2[, 2]))-1
#mmr2[, 3] <- log(mmr2[, 3])
mmr2[, 3] <- max(mmr2[, 3])/(mmr2[, 3])





############
###########
haslo <- "8pxqs9nu6pb1lt46"

library(RMySQL)

# ładujemy sterownik do bazy danych
sterownik <- MySQL()

# aby się połączyć musimy podać użytkownika, hasło i wskazać bazę danych
# inicjujemy połączenie z serwerem bazodanowym
mpolaczenie = dbConnect(sterownik, 
                        user='pbiecek', password=haslo, dbname='students', 
                        host='beta.icm.edu.pl')


dbListTables(mpolaczenie)
dbGetQuery(mpolaczenie, "SELECT count(*) 
           FROM Grabarz_Kosinski_Wasniewski")
# count(*)
# 1     9334



# pobranie wszystkich filmow
movies <- dbGetQuery(mpolaczenie, "SELECT * 
                     FROM Grabarz_Kosinski_Wasniewski")

name_genre <- movies[,c('title','genre')][!duplicated(movies$title),]
name_genre$genre <- stri_extract_first_regex(name_genre$genre,'\\p{l}*')
############
############
#mmrNodes <- data.frame(name=nazwy, group=grupy)
mmrNodes <- name_genre[name_genre$title%in%nazwy,]
mmrNodes <- mmrNodes[order(mmrNodes$title),]
rownames(mmrNodes) <- NULL
names(mmrNodes) <- c('name', 'group')
library(networkD3)
forceNetwork(Links=mmr2, Nodes=mmrNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.9)

