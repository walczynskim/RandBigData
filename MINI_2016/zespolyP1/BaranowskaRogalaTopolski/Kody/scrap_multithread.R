library(stringi)
library(dplyr)
library(tidyr)

########################
# Zadaniem skryptu jest początkowa eksploracja danych wraz z zapisaniem ich
# w przyjaźniejszym formacie. Z logów wyodrębniamy informację o id klienta,
# id sesji, dacie oraz godzinie rozpoczęcia i zakończenia danej sesji. Ponadto dodaję
# flagę, która określa czy użytkownik był aktywny - czyli czy w logu zostało
# zapisane jakiekolwiek polecenie.
# W celu przyspieszenia obliczeń użyty został pakiet parallel, a dokładnie 
# funkcja parallel::parSapply. 
########################

dir <- file.path(getwd(),"RandBigData","Projekt 1", "2013")
files <- list.files(dir, recursive = T, pattern = ".*cnk[0-9]{2}.*\\.log")
files_s <- files[509: 592]
files_s
c1 <- parallel::makeCluster(4)
extract_data <- function(file) {
  library(stringi)
  library(dplyr)
  library(tidyr)
  month = stri_sub(file, 1,2)
  exhibit <- stri_match_all_regex(file, "/(.*)/")[[1]][2]
  
  # omijam stacje, które mają bardzo dużo danych/nieregularną strukturę
  if (exhibit %in% c("cnk29a", "cnk30", "cnk04")) return()
  plik <- file(file.path(getwd(),"RandBigData","Projekt 1", "2013", month, exhibit, paste0(exhibit,".log")))
  dane <- readLines(plik)
  N_dane <- length(dane)
  # funkcje pomocnicze
  is_active <- function(x) {
    # Funkcja patrzy, czy po podejściu użytkownika do eksponatu nastąpiło polecenie
    # zmiany sceny. Jeśli tak, to znaczy że użytkownik wszedł w interakcję z eksponatem - 
    # czyli był aktywny.
    # 
    # W przeciwnym przypadku znaczy, że nie wykonał żadnej akcji (lub pojawił się jakiś błąd). 
    # Wtedy uznajemy go za nieaktywnego.
    #
    return(stri_detect_fixed(dane[as.numeric(rownames(x))+1], "Changing scene"))
  }
  find_end_time <- function(n) {
    # Funkcja znajduje czas odejścia użytkownika od eksponatu. Od momentu logu o 
    # zarejestrowaniu nowego użytkownika sprawdzamy kolejne logi, dopóki są one
    # z kategorii "SCENE/alib". Ostatni log tego typu to na ogół wyświetlenie 
    # tzw. "Splash Screen" - czyli ekran domyślny. Na chłopski rozum oznacza to
    # odejście użytkownika od eksponatu. 
    i=n+1
    while(stri_detect_fixed(dane[i], "SCENE/alib")) {
      i = i+1
      if(i > N_dane) break
    }
    return(stri_sub(dane[i-1], from = 8, to = 15))
  }
  
  # Szukam tych logów, które mówią o nowym użytkowniku
  visitors_begin <- stri_match_all_regex(dane, paste0("(.+) hostname=",exhibit, " INFO: APP/cnklib Added visitor (.+) to session (.+)"))
  # Sprzątanie 
  do.call(rbind, visitors_begin) %>%
    as.data.frame() %>% 
    select(-V1, date_time = V2 , visitor_id = V3, session_id = V4) %>%
    na.omit %>%
    separate(date_time, c("month", "day", "begin_time"), sep = " +") -> visitors_clean
  if(nrow(visitors_clean)==0) {return()}
  # Dodajemy info czy był aktywny, indeks (w celach pomocniczych), i informację
  # o zakończeniu pracy przez danego użytkownika.
  active <- logical(nrow(visitors_clean))
  for(i in 1:nrow(visitors_clean)){
    active[i] = is_active(visitors_clean[i,])
  }
  visitors_clean$is_active <- active
  visitors_clean$index <- as.numeric(rownames(visitors_clean))
  end_time <- numeric(nrow(visitors_clean))
  for(i in 1:nrow(visitors_clean)){
    end_time[i] = find_end_time(visitors_clean[i,]$index)
  }
  visitors_clean$end_time <- end_time
  write.csv(visitors_clean, file.path(getwd(),"RandBigData","Projekt 1", "Raporty", paste0("raport_",exhibit, "_", month,".txt")))
}

start <- Sys.time()
result <- parallel::parSapply(cl=c1,files_s,FUN = extract_data)
stop_t <- Sys.time()
running <- stop_t - start
parallel::stopCluster(c1)
print(running)

result
