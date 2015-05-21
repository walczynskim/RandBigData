#keywords




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

keywordsCorpus <- Corpus(VectorSource(movies$keywords))

keywordsDTM <- DocumentTermMatrix(keywordsCorpus,
                                control = list(tokenize=strsplit_comma_tokenizer))

# przejscie z DTM na macierz rzadka (sparseMatrix), dzieki czemu
# na tym obiekcie mozna w ogole cos policzyc
library(Matrix)
keywordsSparse <- sparseMatrix(i = keywordsDTM$i, j = keywordsDTM$j, 
                             x = keywordsDTM$v, dimnames = keywordsDTM$dimnames)

# specjalna funkcja do liczena sum w kolumnach na macierzy rzadkiej
keywordsColSums <- colSums(keywordsSparse)
# > summary(keywordsColSums)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   11.47    5.00 2583.00


# > dim(keywordsSparse)
# [1]  9334 67682

# wyrzucamy slowa kluczowe, ktore nie pojawily sie w co najmniej 5 filmach
keywordsSparseSmart <- keywordsSparse[,which(keywordsColSums > 5)]

# > dim(keywordsSparseSmart)
# [1]  9334 16049

###################################################################
###################################################################
## Dla tak `malowymiarowej macierzy` moze juz nie jest potrzebne SVD?
###################################################################
###################################################################

# dopasowuje czesciowy rozklad SVD dzieki algorytmowi irlba:
# "Augmented Implicitly Restarted Lanczos Bidiagonalization Methods", J. Baglama and L. Reichel, SIAM J. Sci. Comput. 2005.
# . It is a fast and memory-efficient way to compute a partial SVD.
library(irlba)

keywordsSVD <- irlba( keywordsSparseSmart, nu = 1000, nv = 1000)

# po rozkladzie biore pierwsze 1000 wektorow
keywordsU <- keywordsSVD$u


# dopasowuje macierz odleglosci do obiektu rzedu 9,3 tys x 1 tys
# czas obliczen: okolo poltorej godziny
keywordsDist <- dist( keywordsU )
save( keywordsDist, file = "keywordsDist.rda")




