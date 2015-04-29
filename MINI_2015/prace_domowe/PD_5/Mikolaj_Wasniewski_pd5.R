
library(RMySQL)
library(dplyr)
haslo <- '64tl1bp6un9sqxp8'



# wczytanie pliku z danymi do ramki danych:
Mikolaj_Wasniewski_dane<-read.table("C:/Users/Miko³aj/Documents/SMAD/R and Big Data/wybory/d/a2015-04-06_10_23_28.txt")


polaczenie <- src_mysql(dbname = 'students', 
                        host = 'beta.icm.edu.pl', user = 'pbiecek', 
                        password = haslo)


uchwyt_doTabeli <- copy_to(polaczenie, Mikolaj_Wasniewski_dane, 
                           temporary = FALSE)


# sprawdzenie jakie tabele sa w bazie danych:
sterownik <- MySQL()
mpolaczenie = dbConnect(sterownik, 
                        user='pbiecek', password=haslo, dbname='students', 
                        host='beta.icm.edu.pl')

dbListTables(mpolaczenie)

dbDisconnect(mpolaczenie)


