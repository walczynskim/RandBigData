#######################################################################
### funkcja, ktora sluzy do pobierania danych z portali informacyjnych
#######################################################################

portale_informacyne <- function(portal_www, nodes1, nodes2){
  
  data <- strftime(Sys.time(), "%Y-%m-%d")
  godz <- strftime(Sys.time(), "%H-%M-%S")

  # wczytujemy strone
  portal <- html(portal_www)
  portal_nazwa <- unlist(stri_extract_all_regex(portal_www, "(?<=http://www.).*(?=\\.[^ ]{1,})"))
  # tworzymy potrzebne katalogi
  portal_dir <- stri_paste(getwd(), "/", portal_nazwa)
  if (!file.exists(portal_dir)) dir.create(file.path(portal_dir))
  portal_data <- stri_paste(portal_dir, data, sep="/")
  if (!file.exists(portal_data)) dir.create(file.path(portal_data))
  # nazwa pliku wyjsciowego, zawierajacego cala strone
  nazwa_all <- stri_paste(portal_data, '/tresc', godz, '.txt')
  # zapisujemy
  writeLines(as(portal,'character'),nazwa_all)
  
  cast <- html_nodes(portal, nodes1)
  tytuly <- html_text(cast)
  linki <- html_attrs(cast)
  
  # sprawdzamy czy strona jest o interesujacej nas tematyce  
  tmp <- lapply(tytuly, stri_detect_fixed, slownik)
  wybory <- unlist(linki[sapply(tmp, function(x) any(unlist(x)))])
  # linki do artykulow na temat wyborow
  wynik_link <- as.character(wybory[names(wybory)=='href'])
  wynik_link <-wynik_link[which(stri_detect_regex(wynik_link,'autoplay')==FALSE)]
  wynik_link <-wynik_link[which(stri_detect_regex(wynik_link,'komunikaty')==FALSE)]
  
  wynik_link[which(stri_detect_regex(wynik_link,'http')==FALSE)] <- 
    paste0(portal_www, wynik_link[which(stri_detect_regex(wynik_link,'http')==FALSE)])
  
  # sciagamy zawartoœæ stron z artykulem i j¹ zapisujemy
  for (i in 1:length(wynik_link)){
    godz <- strftime(Sys.time(), "%H-%M-%S")
    artykul <- tryCatch(html(wynik_link[i]), error = function(e) return(invisible(NULL)))
    if(length(artykul)==0) next
    cast <- html_nodes(artykul, nodes2)
    nazwa <- stri_paste(portal_data, '/', godz, '.txt')
    writeLines(html_text(cast),nazwa)
  }
}
