library(dplyr)
library(ggplot2)
library(RColorBrewer)

#dev.off()

# przypisać do urządzenia odpowiednia sciezke z plikiem urzadzenia

urz <- read.csv("C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt 1/ScrappingTextMining-master/Przetworzone/przetworzone_cnk46a.csv",
                stringsAsFactors = F)


############### HEATMAPA Z ROZBICIEM NA TYDZIEN I GODZINY - PROCENTOWA ILOŚĆ ####################

urz %>% select(date, begin_time, diff_time) %>% 
   mutate(hour = format(strptime(begin_time, format="%H:%M:%S"), "%H"),
          weekday = weekdays(as.Date(date)),
          month = format(as.Date(date), "%m")) -> urz_godziny

head(urz_godziny)

suma_calk <- sum(urz$diff_time)
ile_calk <- length(urz$visitor_id)

urz_godziny %>% group_by(weekday, hour) %>%  summarise(ile = n(),
                                                       procent_ile = n()/ile_calk*100) -> urz_godziny


# wszystkie kombinacje godzin i dni tygodnia
godz <- c("00","01", "02", '03', "04", "05", "06", "07", "08", "09", 10:23)
komb <- as.vector(mapply(rep,unique(urz_godziny$weekday),24))
komb <- data.frame(hour = as.character(rep(godz, 7)), weekday = as.character(komb),
                   stringsAsFactors = F)


roznica <-setdiff(komb, urz_godziny[,c("hour", "weekday")])
roznica <- cbind(roznica, ile=rep(0, nrow(roznica)), 
                 procent_ile = rep(0, nrow(roznica)))


urz_godziny <- rbind( urz_godziny, roznica)

urz_godziny$weekday <- as.factor(urz_godziny$weekday)

urz_godziny %>% group_by(weekday) %>% summarise( procent_ile_W = round(sum(ile)/ile_calk*100,1)) -> urz_godziny_GR

# dodaje to do tabeli zagregowanej po dniach,  by potem zrobic kolumne z etykietami: "procent miesiac"

urz_godziny %>% left_join(urz_godziny_GR) -> urz_godziny

# do etykiety na osi Y

urz_godziny$napis <- paste(urz_godziny$procent_ile_W, "%  ", urz_godziny$weekday)
etykiety <- unique(urz_godziny$napis)
etykiety1 <- c(etykiety[2], etykiety[5], etykiety[3], etykiety[1], etykiety[6], 
               etykiety[7], etykiety[4] )

# wybor zakresu godzin

urz_godziny %>% filter(hour %in% c("08", "09", 10:20)) -> urz_godziny_fitr

breaksy <- c(seq(0, max(urz_godziny_fitr$procent_ile), 0.3), 100) 

urz_godziny_fitr$procent_ile_GR <- cut(urz_godziny_fitr$procent_ile, breaks = breaksy ,right = F)

getPalette = colorRampPalette(brewer.pal(9, "GnBu"))

cairo_pdf(file = file.path("C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt 1/plakat_heatmap.pdf"),
          width = 8, height =5)

ggplot(urz_godziny_fitr, aes(x=hour, y=napis)) + geom_tile(aes(fill=procent_ile_GR),color="white", size=0.5, ) + 
   scale_fill_manual(values = getPalette(length(breaksy)), name="zakres   [%]") + 
   scale_y_discrete(limits=etykiety1 ) +
   coord_equal() +
   labs(x="godzina", y="dzień tygodnia", 
        title="Procent całkowitej ilości użyć \n urządzenia cnk46a - Klonowanie") +
   theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14))+
   theme( panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border=element_rect(fill = NA, colour=alpha('black', .5),size=0.5)) +
   theme(plot.title=element_text(size=20))+
   theme(legend.title=element_text(size=12), legend.text=element_text(size=12)) +
   theme(legend.background = element_rect(colour = alpha('black', .5)))

dev.off()
