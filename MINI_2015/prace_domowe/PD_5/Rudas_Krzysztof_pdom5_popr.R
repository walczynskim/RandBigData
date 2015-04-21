library(RMySQL)
library(dplyr)
library(stringi)

haslo<-'64tl1bp6un9sqxp8'


#Ustawiam sciezke
setwd("C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\riduzedane")

pliki<-list.files()
d<-data.frame()
#daję do przesłania około 1/4 plikow z folderu
for(i in 1:ceiling(length(pliki)/4))
{
   temp<-read.table(pliki[i])
   d<-rbind(d,temp)
}

polaczenie <- src_mysql(dbname = 'students', 
                        host = 'beta.icm.edu.pl', user = 'pbiecek', password = nowe_haslo)
uchwyt_doTabeli <- copy_to(polaczenie, d,name="Rudas_wybory", temporary = FALSE)

sterownik <- MySQL()
polacz <- dbConnect(sterownik, 
                         user='pbiecek', password=nowe_haslo, dbname='students', 
                         host='beta.icm.edu.pl')
dbListTables(polacz)#Tabela rzeczywiscie jest
dbDisconnect(polacz)