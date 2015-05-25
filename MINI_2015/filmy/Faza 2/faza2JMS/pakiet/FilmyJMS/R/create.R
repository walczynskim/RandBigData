#' Create a file from a data.frame and saves it.
#'
#' Function \code{create} saves info about actors in a file.
#'
#' @aliases subtitle
#' @param fname Name of a file where info is to be saved.
#' @param frame a data frame with 5 colums
#' @return invisible(NULL)  - all info is gathered in a particular file.
#' @import stringi
#' 
#' 
#' 
create <- function(fname, lista) {
    
    # jesli nie bylo pliku to utworz
    if (!file.exists(fname)) {
        f <- file(fname, open = "a")
        # tworze pierwszy wiersz w pliku:
        writeLines(stri_paste("\"name\"", "\"jobTitle\"", "\"Place_of_Birth\"", 
            "\"BirthDate\"", "\"DeathDate\"", sep = ";"), f)
    } else f <- file(fname, open = "a")
    
    # jesli mamy jakies dane to zapisuj
    if (length(lista$name) > 0) {
        
        for (i in seq_along(lista$name)) {
            # dopisuje do pliku kolejny wiersz
            writeLines(stri_paste(lista$name[i], lista$job[i], lista$birth[i], lista$birth_place[i], 
                lista$death[i], sep = ";"), f)
        }
    }
    close(f)
}
 
