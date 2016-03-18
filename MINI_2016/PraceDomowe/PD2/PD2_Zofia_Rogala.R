#Zadanie domowe 2
#Zofia Rogala

##WYNIK:
##Lista unique.visits składa się z list o nazwach 'raport_OBIEKT_MIESIAC.txt'. Każda z nich składa się z dwóch elementów:
##(1) unique.visitors - liczba unikalnych gości, (2) quantities - tabela mówiąca, ile razy każdy z gości odwiedził daną atrakcję.

library(dplyr)

setwd("~/Dokumenty/Studia")

file.names <- list.files("Raporty")

unique.visits <- list()

for(file.name in file.names){
  raport <- read.csv(paste0("Raporty/",file.name))

    raport %>%
    group_by(visitor_id) %>%
    summarise(quantity = n()) ->
    quantities
    
    unique.visitors <- length(unique(raport$visitor_id))
    
    unique.visits[[length(unique.visits)+1]] <- list(unique.visitors = unique.visitors, quantities = quantities)
    
    names(unique.visits)[length(unique.visits)] <- file.name
}

saveRDS(unique.visits, file = "UniqueVisits.rds")


##Tak odczytuje ten plik RDS
#data <- readRDS("UniqueVisits.rds")


##Zmieniam format daty na bardziej Rowy
# raport$date <- as.Date(paste0(raport$day,"/",'01',"/","2013"), "%d/%m/%Y")

##Sprawdzamy dzien tygodnia:
# weekdays <- cbind(unique(raport$date), strftime(unique(raport$date), "%A"))


