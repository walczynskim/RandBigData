# 2 faza projektu

Przygotowano 3 macierze podobieństw dla filmu


Oparte na

1. `cast`, `director`, `writer` (dla 9334 filmów, DocumenTermMatrix ma to 220 tys kolumn - więc ucięto aktorów, którzy nie występowali w więcej niż 2 filmach i zeszło do 45 tys) - czas obliczeń macierzy odległości 20godzin, 284 MB 
2. `keywords` (dla 9334 filmów, DocumenTermMatrix ma to 65 tys kolumn) - więc ucięto keywords nie występujące przynajmniej w 6 filmach i zredukowano liczbę kolumn do 16 tys) - czas obliczeń macierzy odległości 8godzin, 260 MB 
3. `rating`, `year`, `country`, `language`, `genre`, `column` (około 3 tys kolumn) - - czas obliczeń macierzy odległości 2godzin, 180 MB 


Dla macierzy podobieństw  `1.` i `2.` następnie wykorzystano algorytm `irlba`, czyli uogólnienie rozkładu `SVD` w celu zmniejszenia wymiarowości do 1000 kolumn. Gdyż dopiero na tak pomniejszonym wymiarze funkcja `dist` ma szansę doliczyć macierz podobieńst w skończonym czasie.

Przygotowano w tym celu specjalny pakiet, który dostępny jest wraz z dokumentacją i 3 funkcjami.


Baza danych, do której gromadzono filmy jest dostępna tutaj:

````{Ruby}
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

````