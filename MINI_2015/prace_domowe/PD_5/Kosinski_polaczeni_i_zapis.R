         
> library(RMySQL)
> 
> # ładujemy sterownik do bazy danych
> sterownik <- MySQL()
> 
> mpolaczenie = dbConnect(sterownik, 
+                         user='pbiecek', password=haslo2, dbname='students', 
+                         host='beta.icm.edu.pl')
> dbWriteTable(mpolaczenie, name = "Kosinski_iris", value = iris)
> dbListTables(conn = mpolaczenie)
 [1] "Emilia_Momotko_tabelka_portale" "KWyszynska_GoogleNews"         
 [3] "Kosinski_iris"                  "Martyna_Spiewak_facebook"      
 [5] "Mikolaj_Wasniewski_dane"        "auta2012"                      
 [7] "jankowiak_artykuly"             "jankowiak_podsumowanie"        
 [9] "marta_sommer_tabela_twitter"    "smuda_piotr_fb_likes"          
[11] "sudol_pr_dom"                   "tabelka_rdzanowski" 
