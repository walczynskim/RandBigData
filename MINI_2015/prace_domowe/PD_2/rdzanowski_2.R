require("XML")
require("rvest")
require("stringi")
require("tm")

# zczytuje tekst filmu "MATRIX"

url <- html("http://www.imsdb.com/scripts/Matrix,-The.html")
tekst <-  html_node(url, "pre")
tekst <- html_text(tekst)
#wydobywam "nieobrobione" dialogi
dialogi <- stri_extract_all_regex(tekst, pattern = "\\n\\n\\t\\t\\t\\t\\t.+\\n\\t\\t\\t.+\\n")[[1]]
dialogi2 <- stri_split_fixed(dialogi, "\n\t\t\t")

#z "nieobrobionych" dialogow wydobywam postac i tekst jaki
#ona wypowiada
osoba <- character(0)
tekst <- character(0)

for(i in 1:length(dialogi2)){
  os <- stri_sub(dialogi2[[i]][2], from = 3)
  te <- stri_sub(dialogi2[[i]][3], to = stri_length(dialogi2[[i]][3])-1)
  if(stri_detect_fixed(os, "(V.O.)")){
    os <- stri_sub(os, to = stri_length(os)-7)
  }
  osoba <- c(osoba, os)
  tekst <- c(tekst, te)
}

dialog <- data.frame(osoba = as.factor(osoba), tekst = tekst, stringsAsFactors = FALSE)
# kazdy wiersz ramki przechowuje nazwe osoby, ktora mowi tekst 
# oraz tekst ktory wypowiada


# zobaczmy kto najczesciej mowi w filmie

ile_razy_mowil <- tapply(rep(1, nrow(dialog)), dialog$osoba, sum)
ile_razy_mowil_sort <- sort(ile_razy_mowil)
print(ile_razy_mowil_sort)
# najwiecej mowil Neo, Morfeusz oraz Trinity
barplot(ile_razy_mowil_sort)
pie(ile_razy_mowil_sort)
# piec postaci z Matriksa wypowiedzialo prawie 75% wszystkich kwestii


# zobaczmy kto najwiecej mowil w filmie (kazda litera to jedna jednostka)

ile_mowil <- tapply(stri_length(dialog$tekst), dialog$osoba, sum)
ile_mowil_sort <- sort(ile_mowil)
print(ile_mowil_sort)
# jesli chodzi o dlugosc wypowiedzi najdluzej mowil Morfeusz, nastepnie Neo
# i Trinity
pie(ile_mowil_sort)



# analiza trzech najczesciej mowiacych postaci
w1 <- which(as.character(dialog$osoba) == "TRINITY")
w2 <- which(as.character(dialog$osoba) == "MORPHEUS")
w3 <- which(as.character(dialog$osoba) == "NEO")

tekst_TRINITY <- stri_trans_tolower(stri_paste(dialog$tekst[w1], collapse = " "))
slowa_TRINITY <- stri_extract_all_words(tekst_TRINITY)
tekst_MORPHEUS <- stri_trans_tolower(stri_paste(dialog$tekst[w2], collapse = " "))
slowa_MORPHEUS <- stri_extract_all_words(tekst_MORPHEUS)
tekst_NEO <- stri_trans_tolower(stri_paste(dialog$tekst[w3], collapse = " "))
slowa_NEO <- stri_extract_all_words(tekst_NEO)

# najczesciej wypowiadane slowa przez Neo, TRINITi i MORFEUSZA
corp_NEO <- VCorpus(VectorSource(slowa_NEO))
#usuwam stopwordsy
corp_NEO <- tm_map(corp_NEO, removeWords, stopwords("english")) 
dtm_NEO <- DocumentTermMatrix(corp_NEO)

corp_MORPHEUS <- VCorpus(VectorSource(slowa_MORPHEUS))
#usuwam stopwordsy
corp_MORPHEUS <- tm_map(corp_MORPHEUS, removeWords, stopwords("english")) 
dtm_MORPHEUS <- DocumentTermMatrix(corp_MORPHEUS)

corp_TRINITY <- VCorpus(VectorSource(slowa_TRINITY))
#usuwam stopwordsy
corp_TRINITY <- tm_map(corp_TRINITY, removeWords, stopwords("english")) 
dtm_TRINITY <- DocumentTermMatrix(corp_TRINITY)


# najczesciej wypowiadane slowa przez Neo
print(findFreqTerms(dtm_NEO, 3))

# najczesciej wypowiadane slowa przez TRINITY
print(findFreqTerms(dtm_TRINITY, 3))

# najczesciej wypowiadane slowa przez MORPHEUS
print(findFreqTerms(dtm_MORPHEUS, 3))

