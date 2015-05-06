#sktyp - dzialanie opencpu - wydobycie czestotliwosci literek
library(stringi)
library(httr)
library(opencpu)

#nalezy podac numer serwera, tytul i jezyk
server <- 8771
server <- as.character(server)
title <- "ida"
language <- "pol"


www <- stri_paste("http://localhost:",server,"/ocpu/library/EmoMspi/R/czestotliwosc")

base <- stri_paste("http://localhost:",server,"/ocpu/tmp/")

tit <- stri_paste("'",title,"'")
lan <- stri_paste("'",language,"'")

(info <- httr::POST(www, body = list(tytul=tit,jezyk=lan)))

name <- stri_extract_first_regex(rawToChar(info$content), "x.+(?=/R)")

end1 <- "/R/.val/rda"
stri_paste(base,name,end1)

load(url(stri_paste(base,name,end1)))
(w1 <- .val)

end2 <- "/R/.val/csv"
(w2 <- readLines(url(stri_paste(base,name,end2))))

(tmp <- httr::GET(stri_paste(base,name,"/R")))
(w3 <- cat(rawToChar(tmp$content)))
