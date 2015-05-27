#' Collect info about people
#'
#' Function \code{harvest_people} gets info about people.
#'
#' @aliases subtitle
#' @param htmls list of html objects - parsed HTML pages .
#' @return list with info about people.
#' @import rvest
#'  XML
#'  stringi
#' 
#' 
harvest_people <- function(htmls) {
  
  #ustawiamy zeby dobrze konwertowalo czas
  Sys.setlocale("LC_TIME", "English")
  
  # daty urodzenia
  BIRTH_DEATH <- sapply(htmls, function(x) {
    
    birth <- getNodeSet(x, "//time[@datetime]//a")
    if (is.null(birth)) {
      birthday <- "NA"
      death <- "NA"
    } else {
      dates <- xml_text(birth)
      info <- xml_attr(birth, "href")
      a <- stri_detect_fixed(info, "bth_monthday")
      if (any(a == TRUE)) {
        bmonth_day <- dates[a]
        bmonth <- ""
      } else {
        bmonth_day <- ""
        b <- stri_detect_fixed(info, "bth_month")
        if (any(b == TRUE)) {
          bmonth <- dates[b]
        } else bmonth <- ""
      }
      c <- stri_detect_fixed(info, "bth_year")
      if (any(c == TRUE)) {
        byear <- dates[c]
      } else byear <- ""
      if (byear != "") {
        if (bmonth_day != "") {
          birthday <- stri_flatten(c(bmonth_day, byear), collapse = " ")
          birthday <- as.character(strptime(birthday, "%B %d %Y"))
        } else if (bmonth != "") {
          birthday <- stri_flatten(c(bmonth, 11, byear), collapse = " ")
          birthday <- as.character(strptime(birthday, "%B %d %Y"))
          birthday <- unlist(stri_extract_all_regex(birthday, "[0-9]{4,4}-[0-9]{2,2}"))
        } else {
          birthday <- as.character(byear)
        }
      } else birthday = "NA"
      # daty smierci
      a <- stri_detect_fixed(info, "dth_monthday")
      if (any(a == TRUE)) {
        dmonth_day <- dates[a]
        dmonth <- ""
      } else {
        dmonth_day <- ""
        b <- stri_detect_fixed(info, "dth_month")
        if (any(b == TRUE)) {
          dmonth <- dates[b]
        } else dmonth <- ""
      }
      c <- stri_detect_fixed(info, "dth_year")
      if (any(c == TRUE)) {
        dyear <- dates[c]
      } else dyear <- ""
      if (dyear != "") {
        if (dmonth_day != "") {
          death <- stri_flatten(c(dmonth_day, dyear), collapse = " ")
          death <- as.character(strptime(death, "%B %d %Y"))
        } else if (dmonth != "") {
          death <- stri_flatten(c(dmonth, 11, dyear), collapse = " ")
          death <- as.character(strptime(death, "%B %d %Y"))
          death <- unlist(stri_extract_all_regex(death, "[0-9]{4,4}-[0-9]{2,2}"))
          
        } else {
          death <- as.character(dyear)
        }
      } else death <- "NA"
      
    }
    return(c(birthday, death))
  })
  
  #miejsce urodzenia
  PLACE <- sapply(htmls, function(x) {
    
    place <- getNodeSet(x, "//div[@id='name-born-info']//a[@href]")
    if (!is.null(place)) {
      adress <- xml_attr(place, "href")
      if (length(adress) != 0) {
        where_is_place <- stri_detect_fixed(adress, "birth_place")
        place <- place[where_is_place]
        if (length(place) != 0) {
          place <- xml_text(place, "alt")
          country <- stri_trim_both(unlist(stri_split_regex(place, ",")))
          country <- country[length(country)]
        } else country <- "NA"
        # if(length(country)==0) country='NA'
      } else country = "NA"
    } else country = "NA"
    return(country)
    
  })
  
  #imie i zawod
  IMIE_ZAWOD <- sapply(htmls, function(x) {
    
    p <- getNodeSet(x, "//span[@class='itemprop']")
    if (is.null(p)) 
      return(c("NA", "NA"))
    dane <- xml_text(p)
    nazwy <- xml_attr(p, "itemprop")
    if (length(nazwy) != 0) {
      imie <- dane[which(nazwy == "name")]
      if (length(imie) == 0) 
        imie <- "NA"
      zawod <- dane[which(nazwy == "jobTitle")]
      if (length(zawod) != 0) {
        zawod <- stri_trim_both(zawod)
        zawod <- stri_flatten(zawod, collapse = ", ")
      } else zawod = "NA"
    } else {
      zawod <- "NA"
      imie <- "NA"
    }
    
    return(c(imie, zawod))
  })
  
  #zdejmij nazwy zeby ladniej
  BIRTH_DEATH <- unname(BIRTH_DEATH)
  IMIE_ZAWOD <- unname(IMIE_ZAWOD)
  PLACE <- unname(PLACE)
  
  #zwraca
  list(name=IMIE_ZAWOD[1,],job=IMIE_ZAWOD[2,], birth=BIRTH_DEATH[1,],birth_place=PLACE,death=BIRTH_DEATH[2,])
  
}

