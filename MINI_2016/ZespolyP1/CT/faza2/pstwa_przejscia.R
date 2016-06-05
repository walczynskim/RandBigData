setwd("~/Uczelnia/R i Big Data/CNK/")

library(dplyr)
library(RSQLite)

db <- dbConnect(SQLite(), "dane/dane.db")
dbListTables(db)
dane <- dbReadTable(db, "interakcje")
dbDisconnect(db)


dane$poczatek <- strptime(dane$poczatek, '%Y-%m-%d %H:%M:%S')


# sortowanie w grupach
dane %>% group_by(dzien = as.Date(poczatek), gosc) %>%
    arrange(poczatek) -> dane


eks <- unique(dane$eksponat)
str(dane)

n <- nrow(dane)



# ostatni eksponat w scieżce
dane$ostatni <- c(dane$gosc[-n] != dane$gosc[-1], 1)


# zlicza dla kazdej pary eksponatow (e1, e2) ile razy e2 był w sciezce po e1
tabela <- table(dane$eksponat[-n], dane$eksponat[-1],
                dane$ostatni[-n])
tabela <- tabela[,,1]


licznosci <- table(dane$eksponat, dane$ostatni)[,1]


pstwa <- tabela / licznosci

save(pstwa, file = "pstwa.rda")
