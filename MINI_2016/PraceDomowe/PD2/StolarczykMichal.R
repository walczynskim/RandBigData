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

# sumaryczna liczba użytkowników dla wszytskich eksponatów
dane %>% summarize(sum(ile.wizyt))
# 2566038

# liczba użytkowników dla poszczególnych eksponatów
dane %>% group_by(eksponat) %>% summarize(sum(ile.wizyt))
#    eksponat sum(ile.wizyt)
# 1    cnk02a          65362
# 2    cnk02b          59221
# 3     cnk03          38213
# 4     cnk05          55672
# 5     cnk06          36979
# 6     cnk07          55745
# 7     cnk09          61252
# 8     cnk10          62260
# 9    cnk100          23228
# 10    cnk11          56912
# 11    cnk12          66579
# 12    cnk13          34959
# 13    cnk16          82738
# 14    cnk17          59095
# 15    cnk18          67624
# 16   cnk19a         131096
# 17   cnk19b          81591
# 18    cnk20          54129
# 19    cnk21          48831
# 20    cnk22          57763
# 21    cnk23          40876
# 22    cnk24          58925
# 23    cnk25          35753
# 24    cnk26          29769
# 25   cnk29a          20191
# 26    cnk32          31697
# 27    cnk36             24
# 28    cnk37          37203
# 29    cnk38          26596
# 30    cnk39          44387
# 31    cnk40          45257
# 32   cnk42a          45403
# 33    cnk43          27438
# 34    cnk44          18969
# 35    cnk45          28523
# 36   cnk46a          29468
# 37   cnk46b          35337
# 38    cnk47          34175
# 39   cnk48a          36943
# 40    cnk49          39191
# 41    cnk54          48759
# 42    cnk55          30112
# 43    cnk56          39007
# 44    cnk57          26147
# 45   cnk58b          24232
# 46    cnk59          21951
# 47    cnk60          32639
# 48    cnk61          44644
# 49    cnk62          43740
# 50   cnk63a           4533
# 51    cnk66          55144
# 52    cnk67          47807
# 53    cnk69          47991
# 54    cnk71          29581
# 55    cnk72          41136
# 56    cnk73          29217
# 57    cnk75          35076
# 58   cnk78a          55746
# 59    cnk79          43202

