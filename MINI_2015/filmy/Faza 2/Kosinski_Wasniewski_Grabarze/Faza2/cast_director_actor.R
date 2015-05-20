


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

peopleCorpus <- Corpus(DataframeSource(movies[,c("cast", "writers", "director")]))

peopleDTM <- DocumentTermMatrix(peopleCorpus,
                      control = list(tokenize=strsplit_comma_tokenizer))

# przejscie z DTM na macierz rzadka (sparseMatrix), dzieki czemu
# na tym obiekcie mozna w ogole cos policzyc
library(Matrix)
peopleSparse <- sparseMatrix(i = peopleDTM$i, j = peopleDTM$j, 
                             x = peopleDTM$v, dimnames = peopleDTM$dimnames)

# specjalna funkcja do liczena sum w kolumnach na macierzy rzadkiej
peopleColSums <- colSums(peopleSparse)
# > summary(peopleColSums)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    1.00    2.14    2.00  114.00

# wyrzucamy aktorow, ktorzy nie grali w 3 filmach / albo rezyserow czy scnearzystow
# ktorzy maja dorobek biejdniejszy niz 3
peopleSparseSmart <- peopleSparse[,which(peopleColSums > 2)]


# dopasowuje czesciowy rozklad SVD dzieki algorytmowi irlba:
# "Augmented Implicitly Restarted Lanczos Bidiagonalization Methods", J. Baglama and L. Reichel, SIAM J. Sci. Comput. 2005.
# . It is a fast and memory-efficient way to compute a partial SVD.
library(irlba)

peopleSVD <- irlba( peopleSparseSmart, nu = 1000, nv = 1000)

# po rozkladzie biore pierwsze 1000 wektorow
peopleU <- peopleSVD$u


# dopasowuje macierz odleglosci do obiektu rzedu 9,3 tys x 1 tys
# czas obliczen: okolo poltorej godziny
peopleDist <- dist( peopleU )
save( peopleDist, file = "peopleDist.rda")




