#przetwarzanie i obróbka danych

library(stringi)
library(RSQLite)
library(dplyr)

# wczytaj nazwy wszystkich plików
pliki <- list.files("D:\\MichaÅ‚\\Matematyka\\MUF\\R i Big Data", recursive = TRUE)

# wybierz pliki logów
pliki <- grep(".+\\.log", pliki, value = TRUE)

# pozb¹dŸ siê dziwnych plików (np. 192.168....)
pliki <- grep("cnk", pliki, value = TRUE)

pliki1=grep(basename(p), pliki,  fixed = TRUE, value = TRUE)
hostnames=character()
for (p in pliki){
   
   hostnames=c(hostnames, basename(p))
   
}
hostnames=unique(hostnames)
for(h in hostnames){
   pliki1=grep(h, pliki,  fixed = TRUE, value = TRUE)
   h=strsplit(h,"\\.")[[1]][1]
   dane=data.frame(hostname=character(), user=numeric(), data=numeric())
   for (p in pliki1) {
      
      rok <- strsplit(p, "/")[[1]][1]
      mies <- strsplit(p, "/")[[1]][2]
      linie <- readLines(file.path("D:\\MichaÅ‚\\Matematyka\\MUF\\R i Big Data", p))
      linie <- grep("Added visitor ", linie,  fixed = TRUE, value = TRUE)
      if (length(linie) == 0) next
      user <- stri_extract_first_regex(linie, "(?<=Added visitor )[0-9]+")
      user=as.numeric(user)
      dzien <- as.numeric(substr(linie, 5, 6))
      time <- stri_extract_first_regex(linie, "[0-9]+:[0-9]+:[0-9]+(?= hostname=)")
      data=strptime(paste(rok, mies, dzien, time), "%Y %m %d %H:%M:%S")
      hostname=rep(h, length(user))
      
      dane=rbind(dane, data.frame(hostname=hostname, user=user, data=data))
      
   }
   if (nrow(dane)==0)
      write.table(dane, file.path("D:\\MichaÅ‚\\Matematyka\\MUF\\R i Big Data",paste(h,".txt",sep = "")), row.names = T)
}

#robienie bazy danych

pliki <- list.files("D:\\MichaÅ‚\\Matematyka\\MUF\\R i Big Data")
pliki=pliki[-1]
dane=data.frame(hostname=character(), user=numeric(), data=character(), czas=character())
for (p in pliki){
   
   data1=read.table(file.path("D:\\MichaÅ‚\\Matematyka\\MUF\\R i Big Data", p), skip = 1)
   dane=rbind(dane, data1[,2:5])
   
}
names(dane)=c("hostname", "user", "data", "czas")
czas=strptime(paste(dane$data, dane$czas), format="%Y-%m-%d %H:%M:%S")
czas[1:10]
dane=data.frame(dane$hostname, dane$user, czas)
head(dane)
names(dane)=c("hostname", "user", "czas")
write.csv(dane,file="D:\\MichaÅ‚\\Matematyka\\MUF\\R i Big Data\\ZarombisteDane.txt")
install.packages("RSQLite")

setwd("D:\\MichaÅ‚\\Matematyka\\MUF\\R i Big Data")
sterownik <- dbDriver("SQLite")
polaczenie <- dbConnect(sterownik, "dane.db")
dbWriteTable(polaczenie, "logiCNK", dane)
dbListTables(polaczenie)

nowedane=dbGetQuery(polaczenie, "select hostname, user, substr(czas, 1, 4) as year,substr(czas, 6, 2) as month,substr(czas, 9, 2) as day,time(substr(czas, 12, 8)) as time from logiCNK")
nowedane$day=as.numeric(nowedane$day)
nowedane$month=as.numeric(nowedane$month)
nowedane$year=as.numeric(nowedane$year)
nowedane%>%select(time)%>%filter(time>"12:00:00")%>%head
diff.time=data.frame(year=numeric(), month=numeric(), day=numeric(), hostname=character(), time=numeric())
for (i in 1:12){
   for(j in 1:31){
      daydane=nowedane%>%filter(day==j, month==i)
      users=unlist(daydane%>%select(user)%>%distinct(user))
      print(paste("month ", i, ", day ", j, sep = ""))
      for(u in users){
         htimes=nowedane%>%filter(day==j, month==i, user==u)%>%select(time, hostname)%>%arrange(time)
         if(nrow(htimes)!=0){
            during=diff(strptime(htimes$time, "%H:%M:%S"))
            hostname=htimes$hostname[-nrow(htimes)]
            diff.time=rbind(diff.time, data.frame(year=rep(2013, length(during)), month=rep(i, length(during)), day=rep(j, length(during)), hostname=hostname, time=during))
         }
      }
   }
   write.table(diff.time, paste("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth",i,".txt", sep = ""), row.names = T)
}

timeCNK=read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth1.txt")
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth2.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth3.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth4.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth5.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth6.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth7.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth8.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth9.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth10.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth11.txt"))
timeCNK=rbind(timeCNK, read.table("H:\\Windows7\\Documents\\R\\R i big data\\dane1\\timemonth12.txt"))

dbWriteTable(polaczenie, "timeCNK", timeCNK)