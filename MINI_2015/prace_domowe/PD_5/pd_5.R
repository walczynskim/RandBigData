library(RMySQL)
# candidate_general_info to plik ktory jest aktualizowany przez zewnetrzne funckje
pathTemp <- "F:\\doc\\R i Big Data\\Rproject1\\wybory2\\candidate_general_info.csv"
dfTemp <- read.table(pathTemp, sep = ";", header = TRUE)
sterownik <- MySQL()
mpolaczenie = dbConnect(sterownik, 
                        user='pbiecek', password=haslo, dbname='students', 
                        host='beta.icm.edu.pl')
dbWriteTable(mpolaczenie, value = dfTemp, name = "pawel_grabowski_temp")



dbListTables(mpolaczenie)
# > dbListTables(mpolaczenie)
# [1] "Emilia_Momotko_tabelka_portale" "KWyszynska_GoogleNews"         
# [3] "Kosinski_iris"                  "Martyna_Spiewak_facebook"      
# [5] "Mikolaj_Wasniewski_dane"        "Pawel_Pytlak_tweety"           
# [7] "Rudas_wybory"                   "auta2012"                      
# [9] "jankowiak_artykuly"             "jankowiak_podsumowanie"        
# [11] "katarzyna_fak_twitty"           "marta_sommer_tabela_twitter"   
# [13] "pawel_grabowski_temp"           "smuda_piotr_fb_likes"          
# [15] "sudol_pr_dom"                   "tabelka_rdzanowski"  

dbListFields(mpolaczenie, "pawel_grabowski_temp")
# > dbListFields(mpolaczenie, "pawel_grabowski_temp")
# [1] "row_names"  "TvPInfo"    "WpPl"       "WprostPl"   "OnetPl"     "NewsweekPl" "Tvn24Pl"   
# [8] "NatematPl"  "GazetaPl"   "DziennikPl" "WyborczaPl" "date"       "surname"   
