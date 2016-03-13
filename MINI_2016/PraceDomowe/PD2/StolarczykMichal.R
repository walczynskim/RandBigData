# Skrypt zliczający liczby uzytkowników dla poszczególnych eksponatów w 
# poszczególnych dniach

library("stringi")
library("dplyr")

# wczytaj nazwy wszystkich plików
pliki <- list.files("./dane", recursive = TRUE)

# wybierz pliki logów
pliki <- grep(".+\\.log", pliki, value = TRUE)

# pozbądź się dziwnych plików (np. 192.168....)
pliki <- grep("cnk", pliki, value = TRUE)



# pusta ramka do której będą wstawiane dane
dane <- data.frame(data = numeric(0), 
                   ile.wizyt = numeric(0),
                   eksponat = character(0))


i <- 0
for (p in pliki) {
    i <- i + 1 
    
    eksponat <- strsplit(basename(p), "\\.")[[1]][1]
    rok <- strsplit(p, "/")[[1]][1]
    miesiac <- strsplit(p, "/")[[1]][2]
    
    
    linie <- readLines(file.path("dane", p))
    
    # wybierz linie odpowiadające informacjom o nowych odwiedzających
    linie <- grep("Added visitor ", linie,  fixed = TRUE, value = TRUE)
    
    # jeśli nie znaleziono to idz do nastepnego pliku
    if (length(linie) == 0) next
    
    
    id <- stri_extract_first_regex(linie, "(?<=Added visitor )[0-9]+")
    id <- as.numeric(id)
    
    dzien <- as.numeric(substr(linie, 5, 6))
    data <- strptime(paste(rok, miesiac, dzien), 
                     "%Y %m %d")
    
    # zliczenie liczby unikalnych id w poszczególnych dniach
    # i dołączenie do ramki dane
    data.frame(data, id) %>%
        group_by(data) %>%
        summarise(ile.wizyt = length(unique(id))) %>% # liczby unikalnych id
        cbind(eksponat) %>% 
        rbind(dane, .) ->
        dane

    
    
    # wskaźnik postępu
    cat("\r", i, "/", length(pliki))
    
}


write.csv(dane, "dane.csv")
