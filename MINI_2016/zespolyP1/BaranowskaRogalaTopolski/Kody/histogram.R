#biblioteki      
library(dplyr)
library(ggplot2)
library(stringi)
library(RColorBrewer)

setwd("/home/zosia/studia/RandBigData/Projekt_I/ScrappingTextMining/Przetworzone")


#Wybieram eksponaty do analizy
exhibits <- c("cnk46a", "cnk73", "cnk44")

#Do listy 'dane' pobieram informacje na ich temat, od razu dodajac kolumne 'weekday' (dzienTygodnia)
dane <- list()

for(i in exhibits){
  dane[[i]] <- read.csv(file(paste0("przetworzone_",i,".csv")))
  dane[[i]]$weekday <- strftime(dane[[i]]$date, "%A")
}

#Tworze ramke danych z informacja o sumarycznym czasie uzytkowania dla poszczegolnych urzadzen

##hoursInUse <- roczna sume czasow w sekundach dziele przez 3600, aby otrzymac informacje
##w godzinach oraz #przez ilosc tygodni w roku [a dokladnie przez ilosc np. wtorkow, kiedy
##CNK bylo otwarte]

TimeInUse <- data.frame()

for(i in exhibits){
  dane[[i]] %>% group_by(weekday) %>%
  summarise(visitors = n(), hoursInUse = sum(diff_time)/(3600*length(unique(date)))) %>%
  cbind(Object = i) %>%
  rbind(TimeInUse) ->
  TimeInUse
}

#Sciezka do zapisania wykresu
path <- "/home/zosia/studia/RandBigData/Projekt_I/ScrappingTextMining/WykresyHistogramy/plakat_barplot.pdf"

#Wybieram palete kolorow
getPalette = colorRampPalette(brewer.pal(9, "GnBu"))
  
cairo_pdf(file = path, width = 8, height =5)

  ggplot(data=TimeInUse, aes(x=weekday, y=hoursInUse, fill=Object)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values = getPalette(7)[c(3,5, 7)], name="Numer obiektu") + 
    coord_equal() +
    labs(x=NULL, y="godziny", 
         title="Całkowity czas użytkowania w ciągu dnia") +
    scale_x_discrete(limits=c("poniedziałek", "wtorek", "środa",
                              "czwartek", "piątek", "sobota", "niedziela")) +
    theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14))+
    theme(panel.border=element_rect(fill = NA, colour=alpha('black', .5),size=0.5)) +
    theme(plot.title=element_text(size=20))+
    theme(legend.title=element_text(size=12), legend.text=element_text(size=12)) +
    theme(legend.background = element_rect(colour = alpha('black', .5)))

dev.off()