
source("analiza\\twitter\\wczytanie_calej_tabeli.R")

###############################################################
# slownik stopwordsow:
###############################################################

stop_words <- read.table("polish_stop_words.txt", encoding = "UTF-8")[,1]

###############################################################
# wyciagniecie najczestszych slow z calego tekstu:
###############################################################

male_slowa <- cala_tabela %>%
   "$"("text") %>%
   stri_replace_all_regex("@.*?[:/s]", "") %>%
   stri_extract_all_words() %>%
   unlist() %>%
   stri_trans_tolower()

slowa_bez_stop_words <- male_slowa[!(male_slowa %in% stop_words) & 
                                      !stri_detect_regex(male_slowa, "[0-9]")]

slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "komorow")] <- "komorowski"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "dud")] <- "duda"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "wybor")] <- "wybory"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "glos")] <- "głosowanie"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "głos")] <- "głosowanie"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "prezyden")] <- "prezydent"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "kandydat")] <- "kandydat"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "kampan")] <- "kampania"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "bronis")] <- "bronisław"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "andrze")] <- "andrzej"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "magdalen")] <- "magdalena"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "debat")] <- "debata"
slowa_bez_stop_words[stri_detect_fixed(slowa_bez_stop_words, "tur")] <- "tura"

najczestsze <- slowa_bez_stop_words %>%
   table() %>%
   sort(decreasing = TRUE) %>%
   head(30)

###############################################################
# zapis do pdf:
###############################################################

pdf("analiza//twitter//jeden_wspolny.pdf")

wordcloud(names(najczestsze),  najczestsze, random.order=F, colors="black", 
          min.freq=2, scale=c(3,1))

dev.off()



