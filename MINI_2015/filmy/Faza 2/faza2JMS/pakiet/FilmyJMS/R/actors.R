#' Collect info about actors
#'
#' Function \code{actors} gets info about actors (age,place and date of birth, death).
#'
#' @aliases subtitle
#' @param www A link - see full cast from imdb.
#' @return list with info about actors, moreover all info is saved in csv file
#' @import rvest
#'  XML
#' 
#' 
actors <- function(www) {
    
    # bierzemy selektor
    selector <- c(actor = "//table[@class='cast_list']//a")
    
    # na wypadek gdyby nie bylo polaczenia z internetem lub link nie istnial
    htmls <- tryCatch({
        htmls_movie(www, selector)
    }, error = function(e) {
        "NA"
    })
    
    # jesli html nie zadzialal to koniec
    if (length(htmls) == 1 && is.character(htmls) && htmls == "NA") 
        return("NA")
    
    # zbiera dane
    lista <- harvest_people(htmls)
    
    # zapisuje
    create("Actors.csv", lista)
    
    # zwraca liste
    return(lista$name)
    
} 
