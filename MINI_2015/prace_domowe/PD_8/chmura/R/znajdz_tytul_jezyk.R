#' Znajdowanie linku do napisow filmowych ze strony opensubtitlers.org
#'
#' Funkcja \code{znajdz_tytul_jezyk} jest rozszerzonym pomyslem kolezanki Ady :) Jej zadaniem jest generowanie linku do
#' strony z napisami wskazanego filmu we wskazanym jezyku.
#'
#' @param tytul zmienna typu character, case-insensitive, wskazujaca napisy jakiego filmu nas interesuja.
#' @param jezyk szukanych napisow.
#' @return link do strony z napisami
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
#' @author wersja 1.0.0: Adrianna Sudol, wersja 1.1.0: Katarzyna Fak
#' 
#' @examples
#' znajdz('titanic')
#' znajdz_tytul_jezyk('titanic','pol')
#' 
#' @export

znajdz_tytul_jezyk <- function(tytul, jezyk){
      
      if (missing(tytul) | missing(jezyk))
            stop('Nie podano argumentu')
      url <- paste0('http://www.opensubtitles.org/pl/search2/sublanguageid-', jezyk,'/moviename-')
      title <- strsplit(tytul," ")[[1]]
      sciezka <- paste0(url, ifelse(length(title)==1, title, paste0(title,collapse = "+")))
      return(sciezka)  
}
