#' Collect all info about film,
#'
#' Function \code{wszystko} gets all info about film.
#'
#' @param link A link from imdb.
#' @return List
#' @import rvest
#' @import stringi
#' @import XML
#' 


wszystko = function(link){
  
  film= tryCatch({
    html(link)
  }, error = function(e) {
    'NA'})
  if (is.character(film)&&film=="NA") return("NA")
  
  tytul = wyjmij(film,'.header .itemprop')
  tytul = sprawdz(tytul)
  rok = wyjmij(film,'.header .nobr')
  rok = stri_sub(rok,2,5) # pozbywam sie zbednych nawiasow
  rok = sprawdz(rok)
  czas = wyjmij(film,'time')
  czas = czas[2] # na stronie zawsze sa podane dwa czasy(takie same) wiec biore jeden
  czas = sprawdz(czas)  
  gatunek = wyjmij(film,'.infobar .itemprop')
  gatunek = stri_paste(gatunek,collapse='@')
  gatunek = sprawdz(gatunek)
  data_wydania = wyjmij(film, '.infobar .nobr a')
  data_wydania = sprawdz(data_wydania)
  fabula = wyjmij(film,'#titleStoryLine p')
  fabula = stri_replace_all_regex(fabula,';',',')# srednik bedzie moim separatorem 
  # przy zapisywaniu stad taka zamiana
  fabula = sprawdz(fabula)
  #Motion Picture Rating:
  mpaa = wyjmij(film, '#titleStoryLine .txt-block:nth-child(12) h4+ span')
  mpaa = sprawdz(mpaa)
  mpaa = stri_replace_all_regex(mpaa,';',',')
  kraj = wyjmij(film,'.txt-block:nth-child(4) a')
  if (length(kraj)>1) kraj=stri_paste(kraj,collapse='@')
  kraj = sprawdz(kraj)
  
  nagrody = wyjmij(film,'#titleAwardsRanks')
  if (nagrody!='NA'){
    liczba_nominacji = unlist(stri_extract_all_regex(nagrody,'...nominations'))
    liczba_nominacji = unlist(stri_extract_all_regex(liczba_nominacji,'[0-9]+'))
    liczba_nominacji = sprawdz(liczba_nominacji)
    
    liczba_oscarow = unlist(stri_extract_all_regex(nagrody,'...Oscars'))
    liczba_oscarow = unlist(stri_extract_all_regex(liczba_oscarow,'[0-9]+'))
    liczba_oscarow = sprawdz(liczba_oscarow)
    
    liczba_innych_nagrod = unlist(stri_extract_all_regex(nagrody,'...wins'))
    liczba_innych_nagrod = unlist(stri_extract_all_regex(liczba_innych_nagrod,'[0-9]+'))
    liczba_innych_nagrod = sprawdz(liczba_innych_nagrod)
    
  }else{
    liczba_nominacji='NA'
    liczba_oscarow='NA'
    liczba_innych_nagrod='NA'
  }
  
  dane_oceny = wyjmij(film,'.star-box-details , .star-box-details span')
  
  ocena = dane_oceny[2]
  ocena = sprawdz(ocena)
  max_ocena = dane_oceny[4]
  max_ocena = sprawdz(max_ocena)
  liczba_glosujacych = dane_oceny[5]
  liczba_glosujacych = sprawdz(liczba_glosujacych)
  liczba_recenzji = dane_oceny[6]
  liczba_recenzji = sprawdz(liczba_recenzji)
  
  link_and_title = html_nodes(film, "a")
  links = html_attr(link_and_title, name="href")
  titles = html_text(link_and_title)
  
  wek_pom = stri_detect_regex(titles,'See full cast')
  ktory = which(wek_pom==TRUE)
  w=links[ktory]
  obsada_link = stri_paste(link,w[2])
  if (length(obsada_link)>0 && !is.na(obsada_link)){
    prod_music = get_names(obsada_link)
    
    muzyka = prod_music$music
    muzyka=stri_paste(muzyka,collapse='@')
    muzyka = sprawdz(muzyka)
    
    producenci = prod_music$producers
    producenci = stri_paste(producenci,collapse='@')
    producenci = sprawdz(producenci)
    
    rezyser = director(obsada_link)
    rezyser=stri_paste(rezyser,collapse='@')
    rezyser = sprawdz(rezyser)
    
    aktorzy = actors(obsada_link) 
    aktorzy = stri_paste(aktorzy,collapse='@')
    aktorzy = sprawdz(aktorzy)
  } else{
    muzyka = "NA"
    producenci = "NA"
    rezyser = "NA"
    aktorzy = "NA"
  }
  
  ktory = stri_locate_all_regex(html_text(film),'Budget')
  ktory = unlist(ktory)
  budzet = stri_sub(html_text(film),ktory[1]+7,ktory[2]+28)
  budzet = stri_replace_all_regex(budzet,'\\p{WHITE_SPACE}',' ')
  if (length(ktory)>2){
    budzet = 'NA'
  }
  budzet = sprawdz(budzet)
  
  opis = summary(links,titles,film)
  opis = stri_replace_all_regex(opis,';',',')
  if (length(opis)>1){
    opis = opis[2]
    opis = stri_replace_all_regex(opis,';',',')
  }
  opis = sprawdz(opis)
  
  klucze = wydobadz(links,titles,film,'Keywords','.sodatext') 
  klucze = stri_trim(klucze,'both')
  klucze = stri_paste(klucze,collapse='@')
  klucze = stri_replace_all_regex(klucze,';',',')
  klucze = sprawdz(klucze)
  
  recenzje = wydobadz(links,titles,film,'Reviews','div+ p')
  recenzje = stri_replace_all_regex(recenzje,';',',')
  recenzje = sprawdz(recenzje)
  
  
  id_film =stri_sub(link,27,35)
  id_film = sprawdz(id_film)
  
  lista2 = list(id=id_film,link=link,Title=tytul,Year=rok,Time=czas,Relase=data_wydania,
                Genre=gatunek,Storyline=fabula,MPAA=mpaa,Country=kraj,Ratings=ocena,
                Ratings_max=max_ocena,Users=liczba_glosujacych,Reviews_number=liczba_recenzji,
                Budget=budzet,Keywords=klucze,Description=opis,Oscar=liczba_oscarow,
                Another_awards=liczba_innych_nagrod,Nominations=liczba_nominacji,Music=muzyka,
                Produced=producenci,Actors=aktorzy,Director=rezyser )
  
  
  lista3 = list(id=id_film,text=recenzje)
  create2('filmy.csv',lista2)
  create3('recenzje.csv',lista3)
  
}