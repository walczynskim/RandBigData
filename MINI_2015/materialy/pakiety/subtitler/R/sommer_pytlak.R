#' Analiza liter z napisow do filmu.
#'
#' Funkcja \code{sommer_pytlak} zwraca wykres licznosci liter dla napisow danego filmu w danym jezyku.
#'
#' @param tytul tytul filmu
#' @param jezyk jezyk napisow
#' @return Funkcja \code{sommer_pytlak} zwraca wykres barplot.
#' @details Mozliwe parametry zmiennej \code{jezyk}:
#' \code{all} - wszystkie |
#' \code{pol} - polski |
#' \code{afr} - afrikaans |
#' \code{alb} - albanski |
#' \code{eng}  - angielski |
#' \code{ara} - arabski |
#' \code{arm} - armenski |
#' \code{baq} - basque |
#' \code{bel} - bialoruski |
#' \code{ben} - bengali |
#' \code{bos} - bosniacki |
#' \code{bre} - breton |
#' \code{bul} - bulgarski |
#' \code{bur} - burmese |
#' \code{chi} - chinski uproszczony |
#' \code{hrv} - chorwacki |
#' \code{cze} - czeski |
#' \code{dan} - dunski |
#' \code{dut} - niemiecki |
#' \code{epo} - esperanto |
#' \code{est} - estonski |
#' \code{fin} - finski |
#' \code{fre} - francuski |
#' \code{glg} - galicyjski |
#' \code{geo} - georgian |
#' \code{ell} - grecki |
#' \code{heb} - hebrajski |
#' \code{hin} - hinduski |
#' \code{spa} - hiszpanski |
#' \code{ice} - icelandic |
#' \code{ind} - indonezyjski |
#' \code{jpn} - japonski |
#' \code{cat} - katalonski |
#' \code{kaz} - kazachstanski |
#' \code{khm} - khmer |
#' \code{kor} - koreanski |
#' \code{lit} - litewski |
#' \code{ltz} - luksemburski |
#' \code{lav} - lotyski |
#' \code{mac} - macedonski |
#' \code{may} - malay |
#' \code{mal} - malayalam |
#' \code{mni} - manipuri |
#' \code{mon} - mongolian |
#' \code{mne} - montenegrin |
#' \code{ger} - niemiecki |
#' \code{nor} - norweski |
#' \code{oci} - occitan |
#' \code{per} - perski |
#' \code{por} - portugalski |
#' \code{rum} - rumunski |
#' \code{rus} - rosyjski |
#' \code{scc} - serbski |
#' \code{sin} - sinhalese |
#' \code{slo} - slowacki |
#' \code{slv} - slowenski |
#' \code{swa} - swahili |
#' \code{syr} - syriac |
#' \code{swe} - szwedzki |
#' \code{tgl} - tagalog |
#' \code{tha} - tajlandzki |
#' \code{tam} - tamil |
#' \code{tel} - telugu |
#' \code{tur} - turecki |
#' \code{ukr} - ukrainski |
#' \code{urd} - urdu |
#' \code{hun} - wegierski |
#' \code{vie} - wietnamski |
#' \code{ita} - wloski  |
#' @author Marta Sommer, Pawel Pytlak
#' @import stringi
#' @import dplyr
#' @import rvest
#' @import ggplot2

sommer_pytlak <- function(tytul, jezyk){

  ################################################################################
  ####### czesc pozyczona od Kasi Fak ############################################
  ################################################################################

  znajdz_tytul_jezyk <- function(tytul, jezyk){
    if (missing(tytul) | missing(jezyk))
      stop('Nie podano argumentu')
    url <- paste0('http://www.opensubtitles.org/pl/search2/sublanguageid-', jezyk,'/moviename-')
    title <- strsplit(tytul," ")[[1]]
    sciezka <- paste0(url, ifelse(length(title)==1, title, paste0(title,collapse = "+")))
    return(sciezka)
  }

  linki_i_tytuly_na_stronie <- function(url){
    dostepne_napisy <- html_nodes(html(url),".bnone")
    attrs <- html_attrs(dostepne_napisy)
    hrefs_titles <- sapply(attrs, function(x) x[c("href","title")])
    return(hrefs_titles)
  }

  atrybuty_pobierz <- function(url_napisow){
    pobierz_button <- html_nodes(html(url_napisow),"#bt-dwl")
    attrs2 <- html_attrs(pobierz_button)
    pobierz <- unlist(attrs2)
    return(pobierz)
  }

  pobierz_rozpakuj <- function(pobierz_attrs){
    dir_name <- pobierz_attrs["data-installer-file-name"]
    tmp <- tempfile()
    download.file(paste0(pobierz_attrs["rel"],".zip"),tmp)
    if( dir_name%in%dir() )
      message("Uwaga, nadpisanie istniejacych plikow!") else
        dir.create(dir_name)
    unzip(tmp, exdir=dir_name)
    unlink(tmp)
    message("Napisy pobrano, szefie.")
  }

  pobierz_napisy <- function(tytul, jezyk){
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
      #     if( !all(duplicated(ktory_tytul)[-1]) ){
      #       print(ktory_tytul)
      #       cat("Ktorych napisow potrzebujesz?")
      #       ktory_film <- as.numeric(scan(file = "", what = "", nmax = 1))
      #     }else ktory_film <- 1
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
        zip_url <- paste0("http://opensubtitles.org",hrefs_titles_zagniezdzone["href", 1] )
        pobierz_attrs2 <- atrybuty_pobierz(zip_url)
        if( !is.null(pobierz_attrs2) ){
          pobierz_rozpakuj(pobierz_attrs2)
        }else stop("Nie moge dogrzebac sie do Twoich napisow... ;/")
      }
    }else{
      stop("Nie ma napisow dla tego filmu lub w tym jezyku ;(")
    }
  }

  ################################################################################
  ############ wlasciwy program: #################################################
  ################################################################################

  # tworzymy katalog, gdzie zapisujemy:

  gdzie <- tempdir()
  setwd(gdzie)
  unlink(stri_paste(gdzie, "\\probaprobaproba"), recursive = TRUE, force = TRUE)
  dir.create(stri_paste(gdzie, "\\probaprobaproba"))
  setwd(stri_paste(gdzie, "\\probaprobaproba"))
  pobierz_napisy(tytul, jezyk)
  film <- dir()
  film2 <- dir(film)
  napisy <- film2[which(stri_detect_regex(film2, ".srt$|.txt$"))]

  # zczytujemy napisy:

  tekst <- readLines(stri_paste(film, "\\", napisy))

  tekst %>%
    "["(-c(1, 2, length(.))) %>%
    stri_replace_all_regex("[0-9]", "") %>%
    stri_extract_all_words() %>%
    unlist() %>%
    stri_replace_all_fixed("{Y:i}", "") %>%
    unlist() %>%
    na.omit() -> oczyszczony_tekst

  oczyszczony_tekst[-length(oczyszczony_tekst)] %>%
    stri_paste(collapse=" ") %>%
    stri_trans_tolower() %>%
    stri_extract_all_regex("[a-z\u0105\u0119\u0142\u0144\u00F3\u015B\u017A\u017C]") %>%
    unlist() -> literki

  tabela <- sort(table(literki))

  dane <- data.frame(litery = names(tabela), wartosci = as.numeric(tabela))
  dane$litery <- factor(dane$litery, levels = dane$litery, ordered = TRUE)

  ggplot(dane, aes(x=litery, y=wartosci))+
    geom_bar(colour="black", stat="identity", fill="blue")+
    theme_bw() +
    ggtitle("Jak czeto wystepuja litery w danym filmie?") -> wykres


  # usuwamy stworzone pliki:

  setwd(gdzie)
  unlink(stri_paste(gdzie, "\\probaprobaproba"), recursive = TRUE, force = TRUE)

  return(wykres)
}

