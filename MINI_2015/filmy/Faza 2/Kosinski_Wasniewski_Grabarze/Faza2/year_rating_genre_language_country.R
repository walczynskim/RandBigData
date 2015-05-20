#genre




#Wyciagniecie filmow z bazy danych
load("Faza1/haslo.rda")

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


library(tm)

# stworzenie macierzy wystapien aktorow, rezyserow albo scenarzystow dla tytulow
strsplit_comma_tokenizer <- function(x)
   unlist(strsplit(as.character(x), ",[ ]"))

genreCorpus <- Corpus(DataframeSource(movies[,c("genre","country", "language")]))

genreDTM <- DocumentTermMatrix(genreCorpus,
                                  control = list(tokenize=strsplit_comma_tokenizer))

# przejscie z DTM na macierz rzadka (sparseMatrix), dzieki czemu
# na tym obiekcie mozna w ogole cos policzyc
library(Matrix)
genreSparse <- sparseMatrix(i = genreDTM$i, j = genreDTM$j, 
                               x = genreDTM$v, dimnames = genreDTM$dimnames)






cbind( movies[, c("rating","year")], as.matrix(genreSparse)) -> moviesInfo
# > dim(moviesInfo)
# [1] 9334  287

# malutko

moviesInfoDist <- dist( moviesInfo)
save( moviesInfoDist, file = "moviesInfoDist.rda")




