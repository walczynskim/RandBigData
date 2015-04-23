#


load("haslo.rda")

library(RMySQL)

# ładujemy sterownik do bazy danych
sterownik <- MySQL()

# aby się połączyć musimy podać użytkownika, hasło i wskazać bazę danych
# inicjujemy połączenie z serwerem bazodanowym
mpolaczenie = dbConnect(sterownik, 
                        user='pbiecek', password=haslo, dbname='students', 
                        host='beta.icm.edu.pl')


# przygotowanie bazy danych
#first_row <- MovieInfo("http://www.imdb.com/title/tt1666801/")
 
#dbWriteTable(mpolaczenie, name="Grabarz_Kosinski_Wasniewski", as.data.frame(first_row) )


MoviesUrl <- read.table("C:/Users/Marcin/Desktop/AFTY-DZIUBAS-04-04/MoviesBigDataScience/MoviesUrl.txt", 
                        quote="\"", stringsAsFactors=FALSE)

MoviesUrl <- unlist( MoviesUrl )

lapply( MoviesUrl[1800:length(MoviesUrl)], function( element ){
    
   tryCatch({
         dbGetQuery(mpolaczenie, statement = paste0("insert into Grabarz_Kosinski_Wasniewski (title,original_title,genre,rating,runtime,year,country,language,release_date,budget,gross,opening_weekend,production_co,color,aspect_ratio,sound_mix,director,writers,cast,keywords) VALUES (\"",  tryCatch({ repair_encoding(paste0( unlist(MovieInfo( element )), collapse="\",\""))}, error = function(cond) paste0( unlist(MovieInfo( element )), collapse="\",\"")),  "\")") )
      }, error = function(cond) cat( element ) )
      
})
