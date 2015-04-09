############## (SENSOWNE?) WCZYTANIE artykulow o kandydatach
#######################    ROZNE PORTALE

library("tm")
library("stringi")
library("twitteR")
library("streamR")
library("ROAuth")
library("rvest")

# sciezka dostepu do katalogu, w ktorym skatalogowano foldery z portalami
dir <- "C:\\Users\\Mort\\Desktop\\projekt_r"
# ustawiam katalog roboczy na dir
setwd(dir = dir)
portale <- c("gazeta", "newsweek", "onet", "onet_wybory", "tvn", 
             "tvn_wybory", "tvp", "wp")
# wektor nodesow dla wyszukania glownej informacji z portalu
nodes <- c(".lead", ".Bigfoto1 h2", ".title , #mainPageBody2", ".itemTitle", 
           ".size52", ".Lh1_3", ".rel .title", ".wideArt h2")

tekst <- character()
portal <- character()
data <- character()
waga <- numeric()

for(i in 1:length(portale)){
  por <- portale[i]
  folder_daty <- dir(portale[i])
  for(j in 1:length(folder_daty)){
    dat <- folder_daty[j]
    pliki <- dir(stri_paste(portale[i], folder_daty[j], sep = "\\"))
    for(k in 1:length(pliki)){
      # wczytuje plik txt
      tek <- readLines(stri_paste(portale[i], folder_daty[j], pliki[k], sep = "\\"), 
                         encoding = "UTF-8")
      if(length(tek) > 1){
        if(!stri_detect_fixed(pliki[k], "tresc")){
          tek <- stri_paste(tek, collapse = " ")
          tekst <- c(tekst, tek)
          waga <- c(waga, 1)
          portal <- c(portal, por)
          data <- c(data, dat)
        }
        else{
          html <- html(tek, encoding = "UTF-8")
          tytul <- html_nodes(html, nodes[i])
          if(length(tytul) > 0){
            tytul <- tytul[[1]]
            tytul <- html_text(tytul)
            tekst <- c(tekst, tytul)
            waga <- c(waga, 2)
            portal <- c(portal, por)
            data <- c(data, dat)
          }
        }
      }
    }
  }
}


# kazdy wiersz ramki danych 'dane' zawierac bedzie tekst artykulu,
# nazwe portalu z jakiego pochodzi, data pobrania oraz wage 'waznosci'
dane_portale <- data.frame(tekst = tekst, portal = portal, data = data, 
                           waga = waga, stringsAsFactors = FALSE)

# czyszczenie powtarzajacych sie artykulow
dane_portale <- dane_portale[!duplicated(dane_portale[,c('tekst')]),]


write.csv(dane_portale, "C:\\Users\\Mort\\Desktop\\projekt_r\\eRowe_pliki\\raport_projekt\\dane_portale.csv")
