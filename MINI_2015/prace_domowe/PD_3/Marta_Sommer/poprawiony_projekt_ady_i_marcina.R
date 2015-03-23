######################################################################
############################## PAKIETY ###############################
######################################################################

library("stringi")
library("XML")
library("rvest")
library("rjson")
library("twitteR")
library("ROAuth")
library("streamR")

######################################################################
###################### PRZYDANTNE FUNKCJE ############################
######################################################################

zapis_danych <- function(sciezka_dostepu, nazwa_portalu, co_mam_zapisac, dodatek_do_nazwy_pliku){
  
  sciezka <- stri_paste(sciezka_dostepu, "\\", nazwa_portalu, collapse="")
  if(!file.exists(sciezka)){
    dir.create(sciezka)
  }
  
  data <- as.character(Sys.Date())
  sekunda <- strftime(Sys.time(),"%s")
  
  sciezka2 <- stri_paste(sciezka, "\\", data, collapse="")
  if(!file.exists(sciezka2)){
    dir.create(sciezka2)
  }
  
  sciezka3 <- stri_paste(sciezka2, "\\",sekunda, dodatek_do_nazwy_pliku,".txt")
  file.create(sciezka3)
  
  polaczenie <- file(sciezka3)
  writeLines(co_mam_zapisac, polaczenie)
  close(polaczenie)
}

zapisz_tresc <- function(link_do_portalu, link_do_portalu_glownego, nazwa_portalu, 
                         gdzie_zapisac_dane, selektor, slownik){
  
  # szukam tytulow na stronie:
  
  portal_html <- html(link_do_portalu)
  tresc <- html_nodes(portal_html, 'a')
  tytuly <- html_text(tresc)
  linki <- html_attr(tresc, "href")
  
  # wybieram interesujace mnie tytuly:
  
  linki_ze_slownika <- list()
  for (i in 1:length(slownik)){
    linki_ze_slownika[[i]] <- linki[stri_detect_fixed(tytuly, slownik[i])]
  }
  linki_ze_slownika <- unique(unlist(linki_ze_slownika))
  
  # usuwam linki z filmami:
  
  linki_ze_slownika <- linki_ze_slownika[!stri_detect_fixed(linki_ze_slownika, "autoplay")]
  
  # poprawiam niepoprawne linki:
  
  linki_ze_slownika[!(stri_detect_fixed(linki_ze_slownika, "http://"))] <- 
    stri_paste(link_do_portalu_glownego, 
               linki_ze_slownika[!(stri_detect_fixed(linki_ze_slownika, "http://"))])
  
  # zapis:
  
  for(i in 1:length(linki_ze_slownika)){
    artykul <- tryCatch(html(linki_ze_slownika[i]), error=function(err) NA)
    if(typeof(artykul)=="logical") next
    tresc <- html_nodes(artykul, selektor)
    tresc_text <- html_text(tresc)
    zapis_danych(gdzie_zapisac_dane, nazwa_portalu, tresc_text, stri_paste("_",i,collapse=""))
  }
  
}

######################################################################
############################## SLOWNIK ###############################
######################################################################

sciezka_do_slownika <- "C:\\Users\\Marta\\Dropbox\\R_i_Big_Data\\prace_domowe\\pd3\\slownik.txt"
slownik <- readLines(sciezka_do_slownika)

######################################################################
#################### GDZIE ZAPISAC DANE ##############################
######################################################################

gdzie_zapisac_dane <- "C:\\Users\\Marta\\Dropbox\\R_i_Big_Data\\prace_domowe\\pd3\\dane"

######################################################################
############################ TVN24 ###################################
######################################################################

zapisz_tresc("http://www.tvn24.pl", 
             "http://www.tvn24.pl"
             "tvn",
             gdzie_zapisac_dane, 
             "p , .black", 
             slownik)

######################################################################
###################### TVN24 - WYBORY ################################
######################################################################

zapisz_tresc("http://www.tvn24.pl/wybory-prezydenckie-2015,117,m", 
             "http://www.tvn24.pl",
             "tvn_wybory",
             gdzie_zapisac_dane, 
             "p , .black", 
             slownik)

######################################################################
############################### WP ###################################
######################################################################

zapisz_tresc("http://www.wp.pl", 
             "http://www.wp.pl",
             "wp",
             gdzie_zapisac_dane, 
             "#intertext1 , .lead, .artCont", 
             slownik)

######################################################################
######################### NEWSWEEK ###################################
######################################################################

zapisz_tresc("http://www.newsweek.pl", 
             "http://www.newsweek.pl",
             "newsweek",
             gdzie_zapisac_dane, 
             "p", 
             slownik)

######################################################################
######################### TVP INFO ###################################
######################################################################

zapisz_tresc("http://www.tvp.info", 
             "http://www.tvp.info",
             "tvp_info",
             gdzie_zapisac_dane, 
             ".text , .lead", 
             slownik)

######################################################################
############################# ONET ###################################
######################################################################

zapisz_tresc("http://www.onet.pl/", 
             "http://www.onet.pl/",
             "onet",
             gdzie_zapisac_dane, 
             "p , .black", 
             slownik)

######################################################################
####################### ONET - WYBORY ################################
######################################################################

zapisz_tresc("http://wiadomosci.onet.pl/wybory-prezydenckie/xcnpc", 
             "http://www.onet.pl/",
             "onet_wybory",
             gdzie_zapisac_dane, 
             "p , .black", 
             slownik)

######################################################################
############################## GAZETA ################################
######################################################################

zapisz_tresc("http://gazeta.pl", 
             "http://gazeta.pl",
             "gazeta",
             gdzie_zapisac_dane, 
             "#artykul , #gazeta_article_lead", 
             slownik)
