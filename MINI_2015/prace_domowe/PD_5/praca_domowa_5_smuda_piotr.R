library("RMySQL")
library("dplyr")

haslo<-'64tl1bp6un9sqxp8'

#łączymy się z bazą
polaczenie<-src_mysql(dbname='students',host='beta.icm.edu.pl',user='pbiecek',password=haslo)

#zobaczmy co tam jest w bazie
dbListTables(polaczenie$con)

#ścieżka do folderu z plikami z danymi polubień z fb
sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/projekt - wybory/Facebook/likes"

#pełne ścieżki do plików z polubieniami
pliki_fb_likes<-list.files(sciezka,full.names=TRUE)

#wczytujemy tweety do wektora
smuda_piotr_fb_likes<-do.call(rbind,lapply(pliki_fb_likes,read.csv2))

#wrzucamy plik do bazy
copy_to(polaczenie,smuda_piotr_fb_likes,temporary=FALSE)

# db_drop_table(polaczenie$con,table='smuda_piotr_fb_likes') #by usunąć tabelę

#rozłączamy się z bazą
dbDisconnect(polaczenie$con)
