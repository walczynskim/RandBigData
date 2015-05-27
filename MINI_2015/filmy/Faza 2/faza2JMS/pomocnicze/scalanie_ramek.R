
###########################################################################
##### SCALANIE TABEL #####

#scalamy juz oczyszczone tabele
#używamy readLines, bo dla read.csv to może być za dużo i poucina wiersze

wczytaj_ramke <- function(nazwa) {
   f <- readLines(nazwa)
   f <- stri_split_fixed(f, ";")
   head <- f[[1]]
   n <- length(f)
   df_f <- data.frame(matrix(unlist(f), nrow = n, byrow = T),
                      stringsAsFactors=FALSE)
   colnames(df_f) <- head
   df <- df_f[-1,]
   return(df)
}

filmy <- wczytaj_ramke("filmyClean.csv")
recenzje <- wczytaj_ramke("recenzjeClean.csv")
aktorzy <- wczytaj_ramke("ActorsClean.csv")
rezyserzy <- wczytaj_ramke("DirectorsClean.csv")

#tworzę nową ramkę ze wszystkich 4 poprzez odpowiednie dołączenie kolumn

dodaj_aktorzy_rezyser <- function(filmy, aktorzy, rezyserzy){
   ramka <- cbind(filmy, aktorzy_wiek=0,aktorzy_rozrzut=0,aktorzy_kraje = NA,rezyser_wiek=0,
                  rezyser_kraje=NA)
   n <- nrow(filmy)
   for(i in 1:n){
      print(i)
      ktorzy <- aktorzy_wyszukaj_wiek_kraj4(filmy[i,],aktorzy)
      ramka[i,24] <- median(ktorzy[[1]],na.rm=TRUE)
      ramka[i,25] <- IQR(ktorzy[[1]],na.rm=TRUE)
      kraje <- ktorzy[[2]][which(ktorzy[[2]]!="NA")]
      kraje <- na.omit(kraje)
      if(length(kraje)>0){
         ramka[i,26] <- stri_flatten(na.omit(ktorzy[[2]]),collapse="@")
      }    
      rez <- rezyser_wyszukaj_wiek_kraj4(filmy[i,],rezyserzy)
      kraje <- rez[[2]][which(rez[[2]]!="NA")]
      kraje <- na.omit(kraje)
      ramka[i,27]<-median(rez[[1]],na.rm=TRUE)
      if(length(kraje)>0){
         ramka[i,28] <- stri_flatten(na.omit(rez[[2]]),collapse="@")
      }     
   }
   return(ramka)
}

aktorzy_wyszukaj_wiek_kraj4 <- function(f, aktorzy){
   aktorzy_f <- unlist(stri_split_fixed(f[1,22],"@"))[1:10]
   aktorzy_f <- unlist(stri_split_fixed(f[1,22],"@"))
   l1 <- aktorzy[aktorzy$name%in%aktorzy_f,]
   l1 <- l1[!duplicated(l1$name),]
   if(f[4]!="NA"){
      wiek <- 2015-as.numeric(l1[[3]]) - (2015-as.numeric(f[1,4]))
   } else wiek <- NA
   kraj <- as.character(l1[[4]])
   list(wiek, kraj)
}

rezyser_wyszukaj_wiek_kraj4 <- function(f,rezyserzy){
   rezyserzy_f <- unlist(stri_split_fixed(f[1,23],"@"))
   l1 <- rezyserzy[rezyserzy$name%in%rezyserzy_f,]
   l1 <- l1[!duplicated(l1$name),]
   if(f[4]!="NA"){
      wiek <- 2015-as.numeric(l1[[3]]) - (2015-as.numeric(f[1,4]))
   } else wiek <- NA
   kraj <- as.character(l1[[4]])
   list(wiek,kraj) 
}

nowa <- dodaj_aktorzy_rezyser(filmy, aktorzy, rezyserzy) #trzeba jeszcze zapisac bedzie
nowa.all <- merge(nowa, recenzje, by = "id", all.x = TRUE)
names(nowa.all)[29] <- "recenzje"

write.csv2(nowa.all, "filmyAll.csv", row.names=FALSE)

