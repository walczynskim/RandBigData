library(RMySQL)
library(dplyr)

#ścieżka ustawiona na folder, w którym znajdują się dane pliki tekstowe
ramka1 <- read.csv("artykuly.txt")
ramka2 <- read.csv("podsumowanie.txt")

haslo <- '64tl1bp6un9sqxp8'

polaczenie <- src_mysql(dbname = 'students', 
                        host = 'beta.icm.edu.pl', user = 'pbiecek', password = haslo)

doBazy1 <- copy_to(polaczenie, ramka1, name="jankowiak_artykuly", temporary = FALSE)
doBazy2 <- copy_to(polaczenie, ramka2, name="jankowiak_podsumowanie", temporary = FALSE)

#sprawdzamy
dbListTables(polaczenie$con)

#rozlaczamy
dbDisconnect(polaczenie$con)
