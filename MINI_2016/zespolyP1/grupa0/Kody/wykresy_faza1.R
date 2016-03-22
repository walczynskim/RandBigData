
library(ggplot2)
library(dplyr)
library(reshape2)

sciezka<-getwd()

# lista folderóœ będących miesiącami
foldery <- list.files(file.path(sciezka,"projekt1/dane_csv"))

# utworzenie nazw zmiennych dla ramek danych
# zajmujemy sie tylko trzema wybranymi eksponatami
nam38 <- paste0("dane38_",foldery)
nam39 <- paste0("dane39_",foldery)
nam19a<- paste0("dane19a_",foldery)

# utworzenie ramek danych z danymi dla kazdego eksponatu z calego roku
# na poczatek wczytujemy dane ze wszystkich miesiecy dla kazdego eksponatu
for(i in foldery) { 
   assign(nam38[as.numeric(i)], read.csv( file.path(sciezka,"projekt1/dane_csv",i,"19a","/cnk38_ost.csv" )) )
   assign(nam39[as.numeric(i)], read.csv( file.path(sciezka,"projekt1/dane_csv",i,"19a","/cnk39_ost.csv" )) )
   assign(nam19a[as.numeric(i)], read.csv( file.path(sciezka,"projekt1/dane_csv",i,"19a","/cnk19a_ost.csv" )) )
}


lista_plik38<-lapply(nam38, as.name)
lista_plik39<-lapply(nam39, as.name)
lista_plik19a<-lapply(nam19a, as.name)

# tworze trzy ramki danych dla kazdego urzadzenia
# w ramkach danych mam po 1 obserwacji, dla ktorej czas uztykowania jest ujemny, 
   # wynika to z tego, ze osoba zaczela uzywac eksponatu przed polnoca,
   # a skonczyla po polnocy, wiec usunelam te obserwacje
# pojawial sie tez czas bardzo duzy lub brak danych, rowniez dla jednej obserwacji
cnk39<-do.call(rbind.data.frame,lista_plik39)
cnk39<-cnk39[cnk39$roznica_czas>=0,]

cnk38<-do.call(rbind.data.frame,lista_plik38)
cnk38<-cnk38[cnk38$roznica_czas>=0,]

cnk19a<-do.call(rbind.data.frame,lista_plik19a)
cnk19a<-cnk19a[cnk19a$roznica_czas>=0 & cnk19a$roznica_czas<20000 & !is.na(cnk19a$roznica_czas),]


# wyznaczam sredni czas spedzony przy urzadzeniu w kazdym tygodniu
# na poczatekprzypisuje numer tygodnia w danym roku do kazdego dnia
jaki_tydz_39<-strftime(cnk39$data,'%W')
dane_week_39<-cbind(cnk39,tydzien=jaki_tydz_39)

jaki_tydz_38<-strftime(cnk38$data,'%W')
dane_week_38<-cbind(cnk38,tydzien=jaki_tydz_38)

jaki_tydz_19a<-strftime(cnk19a$data,'%W')
dane_week_19a<-cbind(cnk19a,tydzien=jaki_tydz_19a)

# wyznaczamy srednie czasy dla kazdego eksponatu
sredni_czas_39 <- cnk39 %>% group_by(tydzien) %>% summarise(srednie=mean(roznica_czas))
sredni_czas_38 <- cnk38 %>% group_by(tydzien) %>% summarise(srednie=mean(roznica_czas))
sredni_czas_19a <-cnk19a %>% group_by(tydzien) %>% summarise(srednie=mean(roznica_czas))

# tworzymy wspolna tabele z danymi dla wszystkich eksponatóws
sredni_tydzien_razem <- sredni_tydzien_38 %>% left_join(sredni_tydzien_39,by=c("jaki_tydz_38"="jaki_tydz_39")) %>%
   left_join(sredni_tydzien_19a,by=c("jaki_tydz_38"="jaki_tydz_19a"))
names(sredni_tydzien_razem)<-c("tydzien","cnk38","cnk39","cnk19a")

# przeksztalcamy ramke danych aby wykonac wykres
tydzien_czas_melt<-melt(sredni_tydzien_razem,iv.var="tydzien")
tydzien_czas_melt$tydzien<-as.numeric(as.character(tydzien_czas_melt$tydzien))

# wykres: sredni czas korzystania z eksponatu na tydzien
pdf("sredni_tydzien.pdf")
ggplot(data=tydzien_czas_melt, aes(tydzien,value, col=variable)) +
   geom_line(size=1) +
   labs(x="Tydzień w roku", y="Czas [s]", title='Średni czas (na tydzień)\n spędzony przy urządzeniu') +
   theme(plot.title = element_text(size=17, face="bold", margin = margin(10, 10, 10, 10)),
         axis.title = element_text(face="bold")) +
   scale_colour_manual(values = c("green","cornflowerblue","firebrick1"),name="Eksponat", labels=c("38", "39", "19a")) +
   scale_x_continuous(breaks = seq(0,51,3)) +
   scale_y_continuous(breaks = seq(0,160,10)) 
dev.off()


# policzymy teraz ile osob korzystalo z eksponatow w poszcegolnych dniach tygodnia
# najpierw wyznaczmy liczbe unkalnych id w danym dniu
tmp_unik_38 <- cnk38 %>% group_by(dzien_tyg,data,id) %>%  summarise(ile=n())
licznosc_38 <- tmp_unik_38 %>% group_by(dzien_tyg) %>% summarise(n_odwiedz=n())
# dzielimy licznosc w danym dniu przez liczbe wszystkich uzytkownikow w ciagu roku
licznosc_38$n_odwiedz<- licznosc_38$n_odwiedz/nrow(tmp_unik_38)*100

tmp_unik_39 <- cnk39 %>% group_by(dzien_tyg,data,id) %>%  summarise(ile=n())
licznosc_39 <- tmp_unik_39 %>% group_by(dzien_tyg) %>% summarise(n_odwiedz=n())
licznosc_39$n_odwiedz<- licznosc_39$n_odwiedz/nrow(tmp_unik_39)*100

tmp_unik_19a <- cnk19a %>% group_by(dzien_tyg,data,id) %>%  summarise(ile=n())
licznosc_19a <- tmp_unik_19a %>% group_by(dzien_tyg) %>% summarise(n_odwiedz=n())
licznosc_19a$n_odwiedz<- licznosc_19a$n_odwiedz/nrow(tmp_unik_19a)*100

# tworzymy jedna tabele dla wszystkoch danych
tydzien_razem <- licznosc_38 %>% left_join(licznosc_39, by=c("dzien_tyg"="dzien_tyg")) %>%
   left_join(licznosc_19a, by=c("dzien_tyg"="dzien_tyg"))

# zmieniamy porządek dni tygodnia z alfabetycznego
tydzien_razem$dzien_tyg <- ordered(tydzien_razem$dzien_tyg,
                           levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))
tydzien_razem2<-arrange(tydzien_razem,dzien_tyg)

# tworzymy ramke danych potrzebna do wykresu
tydzien_razem_melt<-melt(tydzien_razem2,iv.var="dzien_tyg")

# oraz sam wykres
pdf("liczba_dzien_tyg_proc.pdf")
ggplot(tydzien_razem_melt, aes(x=dzien_tyg, y=value, fill=variable)) + 
   geom_bar(stat="identity", colour="black", position="dodge") +
   labs(x="Dzień tygodnia", y="Liczba odwiedzających [proc.]", title='Liczba odwiedzających \n w danym dniu tygodnia') +
   theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5),
         plot.title = element_text(size=17, face="bold", margin = margin(10, 10, 10, 10)),
         axis.title = element_text(face="bold")) +
   scale_fill_manual(values = c("green","cornflowerblue","firebrick1"), name="Urządzenie", labels=c("38", "39", "19a"))
dev.off()
