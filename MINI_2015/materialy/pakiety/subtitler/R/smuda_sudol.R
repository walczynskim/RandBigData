#' Pobieranie i wyznaczenie czestosci liter w napisach
#'
#' Funkcja \code{smuda_sudol} pobiera do folderu napisy do filmu ze wskazanym jezykiem oraz wyznacza czestosc
#' liter dla tych napisow
#'
#' @param film zmienna typu character, case-insensitive, wskazujaca napisy jakiego filmu nas interesuja.
#' Tytuly filmow nie musza byc w oryginalnym jezyku.
#' @param jezyk szukanych napisow.
#'
#' @details
#' Funkcja pobiera napisy do filmu przy pomocy funkcji \code{pobierz_napisy}, a nastepnie wczytuje
#' napisy z folderu z napisami i zlicza czestosc uzytych liter w tych napisach.
#'
#' @return Zwraca wektor nazwany z liczba wystapien danych liter z nazw.
#'
#' @author Adrianna Sudol, Piotr Smuda
#'
#' @examples
#' smuda_sudol("Titanic","pol")
smuda_sudol<-function(film,jezyk){
  pobrane<-pobierz_napisy_ss(film,jezyk)
  sciezka<-list.files(pobrane,recursive = TRUE, full.names = TRUE)
  sciezka<-sciezka[stri_detect_regex(sciezka,'(\\.srt$|\\.txt$)')]
  napisy<-readLines(sciezka)
  litery<-stri_extract_all_regex(napisy,'\\p{L}')
  litery<-stri_trans_tolower(unlist(litery))
  litery<-litery[!(is.na(litery))]
  wynik<-sort(table(litery),decreasing = TRUE)
  return(wynik)
}



pobierz_napisy_ss<-function(tytul, jezyk){
  if (missing(tytul) | missing(jezyk))
    stop('Nie podales wszystkich argumentow')
  url <- znajdz_tytul_jezyk(tytul, jezyk)
  # wczytanie wyszukanych na stronie url linkow i tytulow filmow. Funkcje wykonamy, jesli jakiekolwiek
  # sie znalazly.
  hrefs_titles <- linki_i_tytuly_na_stronie(url)
  if( length(hrefs_titles)>0 ){
    ktory_tytul <- hrefs_titles["title",]
    # Co jesli uzytkownik podal film "Shrek" a program nie wie, czy chodzilo mu o
    # Shrek - Shrek, czy "Shrek Forever" czy "Shrek 2"?
    ktory_film <- 1
    url_napisow <- paste0("http://opensubtitles.org", hrefs_titles["href", ktory_film])
    pobierz_attrs <- atrybuty_pobierz(url_napisow)
    if( !is.null(pobierz_attrs) ){
      pobierz_rozpakuj(pobierz_attrs)
      # Normalnie napisy powinny pobrac sie juz w tym kroku, jednak czesto na stronie
      # opensubtitles.org trzeba kliknac w jeszcze jeden link np. dla filmow
      # "La vita e bella" lub "Titanic". Jezeli atrybuty pobierania w tym kroku
      # ustawione sa na NULL (pomimo, ze stri_detektor wykryl film) nalezy wykonac
      # jeszcze jedno przejscie wglab strony.
    }else{
      hrefs_titles_zagniezdzone <- linki_i_tytuly_na_stronie(url_napisow)
      if(length(hrefs_titles_zagniezdzone)==0)
        stop("Niedokladne zapytanie, mozliwe, ze szukasz serialu?")
      # czy_znaleziono_tytul <- stri_detect_regex(tolower(href_n_title[2,]), tolower(tytul))
      # nie trzeba sprawdzac, czy znaleziono tytul, bo na to juz wskazuje sam fakt ze sie wyszukalo.
      zip_url <- paste0("http://opensubtitles.org",hrefs_titles_zagniezdzone["href", 1]  )
      pobierz_attrs2 <- atrybuty_pobierz(zip_url)
      if( !is.null(pobierz_attrs2) ){
        pobierz_rozpakuj(pobierz_attrs2)
      }else stop("Nie moge dogrzebac sie do Twoich napisow... ;/")
    }

  }else{
    stop("Nie ma napisow dla tego filmu lub w tym jezyku ;(")
  }
}


