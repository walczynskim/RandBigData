logi=dbReadTable(polaczenie, "logiCNK")
danki=dbReadTable(polaczenie, "timeCNK")
tyg=logi%>%mutate(dzientyg=weekdays(as.Date(czas)))
danki=danki%>%mutate(dzientyg=weekdays(as.Date(paste0(year,"-",month,"-",day))))
tyg=danki%>%mutate(dzientyg=weekdays(as.Date(paste(year, month, day, sep="-"))))
ile_dzien=tyg%>%select(dzientyg, time)%>%group_by(dzientyg)%>%summarise(laczny_czas=sum(time))
ile_osob=danki%>%select(dzientyg)%>%group_by(dzientyg)%>%summarise(ilosc_logowan=n())
koniec<-left_join(ile_osob,ile_dzien)
koniec
nowe=koniec[c(4,7,6,1,3,5,2),]
library(plotrix)
library("colorspace")
library(RColorBrewer)

kolory<-c("#FFFF80FF","#FFFF00FF","#FFFF00FF","#FF8000FF","#FF8000FF",
          "#FF0000FF","#FF8000FF")
kolory2<-c("#FFFF80FF","#FFFF00FF","#FF8000FF","#FF0000FF")

slices<-nowe$ilosc_logowan
lbls<-nowe$dzientyg
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices,labels = lbls, col=kolory,explode=0.1,
    main="IloĹ›Ä‡ logowaĹ„ w zaleĹĽnoĹ›ci od dnia tygodnia ")

legend("top",
       c("poniĹĽej 20000h","20000-25000h","25000-30000h", "powyĹĽej 30000h"),
       cex=0.8, fill=kolory2,
       title="Liczba godzin spędzonych przy urządzeniach",
       title.adj=1)


