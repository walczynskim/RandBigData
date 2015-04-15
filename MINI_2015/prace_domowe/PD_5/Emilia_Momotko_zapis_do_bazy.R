library(stringi)
library(dplyr)

setwd("C:\\Users\\Emilka\\Documents\\RRR")

pliki <- list.files(pattern=".csv")

ramka <- data.frame()

for(i in seq_along(pliki)){
  
  ramka <- rbind(ramka, read.csv2(pliki[i]))
  
}

haslo <- '64tl1bp6un9sqxp8'

polaczenie <- src_mysql(dbname = 'students', 
                        host = 'beta.icm.edu.pl', user = 'pbiecek', password = haslo)

kopiuj <- copy_to(polaczenie, ramka, name="Emilia_Momotko_tabelka_portale",temporary = FALSE)

