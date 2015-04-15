library("RMySQL")
library("stringi")

# wczytuje moja tabele z projektu 1
tabelka_rdzanowski <- read.csv("C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane_tweet_kandydat.csv")


haslo <- stri_reverse('64tl1bp6un9sqxp8')
sterownik <- MySQL()
# lacze sie z baza danych
mpolaczenie <- dbConnect(sterownik, 
                         user='pbiecek', password = haslo, dbname='students', 
                         host='beta.icm.edu.pl')

# zapisuje tabele tabelka_rdzanowski do bazy danych
dbWriteTable(con = mpolaczenie, name = "tabelka_rdzanowski", 
             value = tabelka_rdzanowski)

# sprawdzam, czy jest ok
dbListTables(mpolaczenie)

# rozlaczam sie z baza
dbDisconnect(mpolaczenie)
