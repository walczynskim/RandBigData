library("dplyr")
library("RMySQL")

# haslo (ma odwrocone znaki):

haslo <- '64tl1bp6un9sqxp8'

# wczytanie mojej tabeli:

sciezka_dostepu <- "C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\web-scraping\\indicators\\twitter\\dane\\tabela.txt"

marta_sommer_tabela_twitter <- data.frame(do.call(rbind,
                                          strsplit(readLines(sciezka_dostepu)[-1],
                                                   "***moj_cudowny_separator***",
                                                   fixed=TRUE))[, -c(1,2)])
colnames(marta_sommer_tabela_twitter) <- c("text", "favorited", "favoriteCount", "replyToSN",
                                   "created", "truncated", "replyToSID", "id", "replyToUID",
                                   "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted",
                                   "longitude", "latitude") 

marta_sommer_tabela_twitter <- marta_sommer_tabela_twitter[-which(!marta_sommer_tabela_twitter$favorited %in% c(TRUE, FALSE)), ]
marta_sommer_tabela_twitter <- marta_sommer_tabela_twitter[,1:16]

# lacze sie z baza danych:

polaczenie <- src_mysql(dbname = 'students', 
                        host = 'beta.icm.edu.pl', user = 'pbiecek', password = nowe_haslo)

# zapis tabeli do bazy:

uchwyt_doTabeli <- copy_to(polaczenie, marta_sommer_tabela_twitter, temporary = FALSE)

# spr, czy tabela jest w bazie:

# nowe polaczenie:

sterownik <- MySQL()
mpolaczenie <- dbConnect(sterownik, 
                        user='pbiecek', password=nowe_haslo, dbname='students', 
                        host='beta.icm.edu.pl')

# lista wszystkich tabel:

dbListTables(mpolaczenie)

# nazwy kolumn w mojej tabeli:

dbListFields(mpolaczenie, "marta_sommer_tabela_twitter")

# odczyt tabeli z bazy danych:

tabela <- tbl(polaczenie, "marta_sommer_tabela_twitter")
head(tabela)

# jak usunac tabele z bazy danych:
# db_drop_table(mpolaczenie, "marta_sommer_tabela")

