#' Pobierz charakterystyki filmow z roznych portali.
#'
#' Funkcja \code{znajdz_info_o_filmie} szuka informacji o filmach na Wikipedii, Opensubtitles lub IMDB.
#'
#' @param tytul tytul filmu
#' @param rok_produkcji rok_produkcji filmu
#' @param w_jakim_folderze_zapisac sciezka do folderu, w ktorym chcemy zapisac pobrane dane (zapisza sie w formacie uproszczonytytul_imdb.txt, uproszczonytytul_opensubtitles.txt lub uproszczonytytul_wikipedia.txt)
#' @param jaki_portal wektor napisow (do wyboru: "imdb", "wikipedia", "opensubtitles")
#' @return Funkcja \code{znajdz_info_o_filmie} zapisuje maksymalnie trzy pliki .txt (kazdy zawiera ramke danych) w wybranym przez nas folderze.
#' @author Marta Sommer
#' @import stringi
#' @import dplyr
#' @import rvest
#' @import RCurl
#' @import XML

znajdz_info_o_filmie <- function(tytul, rok_produkcji, w_jakim_folderze_zapisac, jaki_portal){
  
  uprosc_tytul <- function(wektor_tytulow){
    
    wektor_tytulow %>%
      stri_replace_all_fixed("\u0105", "a") %>%
      stri_replace_all_fixed("\u0107", "c") %>%
      stri_replace_all_fixed("\u0119", "e") %>%
      stri_replace_all_fixed("\u0142", "l") %>%
      stri_replace_all_fixed("\u0144", "n") %>%
      stri_replace_all_fixed("\u00F3", "o") %>%
      stri_replace_all_fixed("\u015B", "s") %>%
      stri_replace_all_fixed("\u017A", "z") %>%
      stri_replace_all_fixed("\u017C", "z") %>%
      stri_replace_all_fixed("\u00FC", "u") %>%
      stri_replace_all_fixed("\u00E7", "c") %>%
      stri_trans_tolower() %>%
      stri_extract_all_words() %>%
      lapply(stri_paste, collapse="") %>%
      unlist()
    
  }

  # imdb
  
  if("imdb" %in% jaki_portal){
  
  skad_brac_link <- stri_paste("http://www.imdb.com/search/title?count=250&release_date=",
                               rok_produkcji, ",", rok_produkcji, "&title=", tytul, 
                               "&title_type=feature&view=simple")

  strona <- html(skad_brac_link)
  
  strona_nodes <- html_nodes(strona, ".title a")
  
  tytuly <- html_text(strona_nodes)
  uproszczony_tytul <- uprosc_tytul(tytuly)
  ktore <- which(uproszczony_tytul == uprosc_tytul(tytul))
  
  linki <- stri_paste("http://www.imdb.com", html_attr(strona_nodes, "href"))[ktore]
  
  tabela_tytulow <- data.frame(tytul=tytuly[ktore], link=linki)
  wskazniki <- data.frame()
  
  for(i in 1:nrow(tabela_tytulow)){
    
    link <- as.character(tabela_tytulow$link[i]) %>% stri_replace_all_fixed("\"", "'")
    strona <- html(link)
    
    tytul <- as.character(tabela_tytulow$tytul[i])
    rok_produkcji <- strona %>% html_nodes(".header a") %>% html_text() %>% as.numeric()
    czas_trwania <- strona %>% html_nodes("time") %>% html_text() %>% "["(2) %>% stri_extract_all_regex("[0-9]+") %>% unlist() %>% as.numeric()
    ocena <- strona %>% html_nodes("strong span") %>% html_text() %>% as.numeric()
    opis <- strona %>% html_nodes("#titleStoryLine p") %>% html_text() %>% ifelse(length(.)==0, NA, .) %>% stri_replace_all_fixed("\"", "'")
    
    wskazniki <- rbind(wskazniki, 
                       data.frame(tytul=tytul, rok_produkcji=rok_produkcji, czas_trwania=czas_trwania, ocena=ocena, opis=opis))
    
  }
  
  write.table(wskazniki, stri_paste(w_jakim_folderze_zapisac, "\\", uprosc_tytul(tytul), "_imdb.txt"), 
              quote = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  }
  
  # wikipedia

  if("wikipedia" %in% jaki_portal){

  key_word <- stri_paste(stri_replace_all_fixed(tytul, " ", "+"), "+film+", rok_produkcji, "+wikipedia")
  
  key_word %>%
    stri_paste("http://www.google.pl/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=", .) %>%
    getURL() %>%
    htmlParse() %>%
    xpathApply(., "//h3//a[@href]", function(x) xmlAttrs(x)[[1]]) %>%
    "[["(1) %>%
    strsplit("?q=") %>%
    unlist() %>%
    "["(2) %>%
    strsplit("&sa") %>%
    unlist() %>%
    "["(1) -> link 
  
  strona <- html(link)
  
  tabela <- readHTMLTable(strona)[[1]][-1, ] %>% 
    rbind(data.frame(V1 = "Tytul", V2 = tytul), .) 
  if(length(which(is.na(tabela[,2])))!=0) tabela <- tabela[1:which(is.na(tabela[,2]))[1]-1, ] 
  
  write.table(tabela, stri_paste(w_jakim_folderze_zapisac, "\\", uprosc_tytul(tytul), "_wikipedia.txt"), 
              quote = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)

  }
  
  if("opensubtitles" %in% jaki_portal){
    # opensubtitles
    
    key_word <- stri_paste(stri_replace_all_fixed(tytul, " ", "+"), "+film+", rok_produkcji, "+opensubtitles")
    
    key_word %>%
      stri_paste("http://www.google.pl/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=", .) %>%
      getURL() %>%
      htmlParse() %>%
      xpathApply(., "//h3//a[@href]", function(x) xmlAttrs(x)[[1]]) %>%
      "[["(1) %>%
      strsplit("?q=") %>%
      unlist() %>%
      "["(2) %>%
      strsplit("&sa") %>%
      unlist() %>%
      "["(1) -> link 
    
    strona <- html(link)
    
    html_nodes(strona, "fieldset p") %>% 
      html_text() %>% 
      stri_replace_all_regex("[\\t\\n]", "") %>% 
      stri_replace_all_regex("\"", "'") %>% 
      "["(-length(.)) %>% 
      stri_split_fixed(":") -> tabela
    tabela[[1]][2] <- tabela[[1]][1]
    tabela[[1]][1] <- "Opis"
    
    wskazniki <- data.frame()
    
    for(i in 1:length(tabela)){
      wskazniki <- rbind(wskazniki, data.frame(v1=tabela[[i]][1], v2=tabela[[i]][2]))
    }
    
    write.table(wskazniki, stri_paste(w_jakim_folderze_zapisac, "\\", uprosc_tytul(tytul), "_opensubtitles.txt"), 
                quote = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  }
  
}

# tytul <- "Titanic"
# rok_produkcji <- 1997
# w_jakim_folderze_zapisac <- "C:\\Users\\Marta\\Desktop\\proba"
# jaki_portal <- c("wikipedia", "imdb", "opensubtitles")
# 
# znajdz_info_o_filmie(tytul, rok_produkcji, w_jakim_folderze_zapisac, jaki_portal)
  
