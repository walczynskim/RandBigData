#######################################
### facebook.com
#######################################

facebook_wskazniki <- function(){

  require(dplyr)
  require(tidyr)
  require(stringi)
  require(data.table)
  
  #######################################
  
  setwd("C:\\Users\\MARTYNKA\\Desktop\\R i BIG DATA\\funkcje")
  
  source("facebook_podsumowanie.R")
  
  ### liczba "lajków" dla ka¿dego z kandydatów
  likes_per_day()
  
  ### analiza sentymentu z komentarzy z facebooka
  ### stystyki zapisujemy do pliku 
  ### sentiment_imie_nazwisko_facebook_comments
  ### gdzie kolumny "neutral" "possitive" "negative" "comments_like_counts"
  ### oznaczaja sume komentarzy o wyzdzieku neutralnym/pozytywnym/negatywnym
  ### dla ka¿dego postu
  
  facebook_sentiment_podsumowanie()
  
  ### srednia liczba polubien, komentarzy oraz udostepnien 
  ### statystyki zapisujemy do pliku 
  ### mean_imie_nazwisko_facebook
  ### gdzie kolumny "likes_count" "comments_count" "shares_count"
  ### oznaczaja srednia liczbe polubien strony na temat danego kandydata, 
  ### srednia liczbe komentarzy pod postem umieszczonym na stronie 
  ### oraz srednia liczbe odestpnien postu
  
  facebook_posts_podsumowanie()
}
  
