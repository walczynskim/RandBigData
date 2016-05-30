#biblioteki      
library(stringi)
library(dplyr)
library(tidyr)

#ustalam wczesniej miesiąc i numer wystawy - przyda się przy pozniejszej automatyzacji
month <- "01"
exhibit <- "cnk02a"

plik <- file(file.path(getwd(), "studia", "RandBigData", "Projekt_I",
                       "ScrappingTextMining", "Raporty", paste0("raport_",exhibit,"_",month,".txt")))

dane <- read.table(plik , sep=",", header=TRUE)

#tworze kolumne z datami w formacie R-owym
dates <- double(nrow(dane))
for(i in 1:nrow(dane)){
  dates[i] = paste0(dane$day[i],"/",month,"/","2013")
}
dane$date <- as.Date(dates, "%d/%m/%Y")

## wypisuje sie dzien tygodnia po polsku 
## dane$weekday <- strftime(dane$date, "%A")

#  wypisuje sie dzien jako numer, 1 - poniedzialek, ..., 7 - niedziela 
dane$weekday <- strftime(dane$date, "%u")
