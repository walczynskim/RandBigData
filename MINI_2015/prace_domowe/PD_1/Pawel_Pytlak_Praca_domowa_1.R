##------------------------------------------
library(rvest)
library(stringi)

precious <- html(
   "http://www.imdb.com/title/tt0929632/reviews?count=260&start=0")
##jako przyklad wzialem recenzje do filmu "Hej Skarbie"

#w tej czesci pozbywamy sie wszystkich recenzentow, ktorzy nie podali 
#miejsca swojego pochodzenia
dane_rec <- html_nodes(precious, "#tn15content a , small:nth-child(7), 
                       small:nth-child(5), small:nth-child(9)")
dane_rec <- html_text(dane_rec)
dane_rec <- dane_rec[dane_rec!=""] #pozbywam sie pustych napisow

dane_rec <- dane_rec[2:(length(dane_rec)-13)] #usuwam niepotrzebne elementy 
#wektora odpowiadajace linkom do innych stron na IMDB (sa ta wspolne 
#elementy dla kazdego recenzowanego filmu) 

wyszukaj_daty <- stri_detect(dane_rec, regex = 
               "^[0-9]{1,2}[[:space:]][A-Z]{1}[a-z]+[[:space:]][0-9]{4}")
dane_rec[wyszukaj_daty == TRUE] #lista elementow w naszym wektorze bedacymi
#datami recenzji
dane_rec <- dane_rec[wyszukaj_daty == FALSE] #usuwam daty z naszego wektora

wyszukaj_kraje <- stri_detect(dane_rec, regex = "^from[[:space:]][A-Za-z]")
#wykrywam elementy wskazujace na miejsce pochodzenia recenzenta
#wyszukaj_kraje <- as.character(wyszukaj_kraje)
wyszukaj_kraje
indeksy_falsz <- which(wyszukaj_kraje == FALSE) #indeksy elementow o 
#wartosciach FALSE, tzn, odpowiadajace nazwom recenzentow
dane_rec[indeksy_falsz] #lista wszystkich nazw recenzentow

if(wyszukaj_kraje[length(wyszukaj_kraje)] == FALSE){
   dane_rec <- dane_rec[-length(wyszukaj_kraje)]
} #jesli ostatnim elementem jest nazwa recenzenta (czyli recenzent nie 
#podal miejsca swojego pochodzenia), to usuwam go

o_jeden_mniej <- dane_rec[indeksy_falsz-1]
o_jeden_mniej
wysz_kraje_2 <- stri_detect(o_jeden_mniej, regex = 
                               "^from[[:space:]][A-Za-z]")
wyrzucamy <- o_jeden_mniej[wysz_kraje_2 == FALSE] #lista nazw recenzentow, 
#ktorzy nie podali miejsca swego pochodzenia
indeksy_do_wyrzucenia <- which(dane_rec %in% wyrzucamy) 
dane_rec <- dane_rec[-indeksy_do_wyrzucenia] #wektor po usunieciu 
#recenzentow bez informacji o miejscu swego pochodzenia
dane_rec <- dane_rec[-393] #dane po usunieciu miejsca pochodzenia 
#recenzenta bez nazwy
dane_rec

wyszukaj_kraje_3 <- stri_detect(dane_rec, regex = 
                                   "^from[[:space:]][A-Za-z]")
wyszukaj_kraje_3
data.frame(Uzytkownik = dane_rec[wyszukaj_kraje_3 == FALSE], 
           Miejsce = dane_rec[wyszukaj_kraje_3 == TRUE])
#informacje podane w formie ramki danych

##---------------------------
#w tej czesci wyciagamy oceny recenzentow, ktorzy zdecydowali sie postawic
#konkretna note filmowi (na stronie podane jako obrazki: zamalowane 
#gwiazdki)
ocena_rec <- html_nodes(precious, "img:nth-child(3) , img:nth-child(5)")
jako_string <- sapply(ocena_rec, function(x) as(x, "character"))
oceny <- unlist(lapply(jako_string, 
                       function(y) stri_extract_all_regex(y, "[0-9]+/10")))
oceny







































