######################################################
### Praca domowa 5
### Martyna Œpiewak
######################################################

path <- "C://Users//MARTYNKA//Documents//Wybory//Facebook"

library(RMySQL)
library(dplyr)

sterownik <- MySQL()
polaczenie <- src_mysql(dbname = 'students', 
                        host = 'beta.icm.edu.pl', user = 'pbiecek', password = haslo)

# lacze wszystkie posty z facebooka w jedna tabelke:
kandydant_likes <- list.files(path = path, pattern="_facebook.csv")
table <- data.frame()

for(name in kandydant_likes){
  tmp <- read.table(paste(path, name, sep="/"), sep=";", head = TRUE)
  table <- rbind(table, tmp)
}
# zmieniam format
table <- tbl_df(table)
# zapisuje do bazy
copy_to(polaczenie, table, name = "Martyna_Spiewak_facebook", temporary = FALSE)

