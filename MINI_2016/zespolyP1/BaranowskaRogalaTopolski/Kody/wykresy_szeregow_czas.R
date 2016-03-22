library(dplyr)
library(stringi)
library(ggfortify)
library(ggthemes)
library(reshape2)

################################
# Zadaniem skryptu jest wygenerowanie wykresu pod tytułem 
# 'Trend w średnim czasie odwiedzin dla wybranych stacji'.
# Inspiracją dla tego wykresu był wpis na blogu SmarterPoland na temat
# analizy danych z krokomierza. Tutaj robimy rzecz analogiczną - rozbijamy 
# dane dotyczące średnich czasów odwiedzin w celu zobaczenia pewnych zależności.
# 
# Poza samym pakietem ggfortify, użyty został również pakiet reshape2, w celu
# pogrupowania zmiennych do naniesienia ich na wykres.
################################

ex1 <-"cnk46a"
ex2 <- "cnk73"
ex3 <- "cnk44"
exs <- c(ex1, ex2, ex3)
path <- file.path("C:", "Users", "Bartek.bartek-pc", "Documents", "RandBigData", "ScrappingTextMining")
ramka = NULL

for (k in 1:3) {
  i <- exs[k]
  # wczytujemy plik
  raport <- read.csv(file.path(path, "Przetworzone",paste0("przetworzone_", i, ".csv")))
  
  # filtrujemy godziny otwarcia
  raport %>%
    filter(as.numeric(stri_sub(begin_time, 1,2))< 19, as.numeric(stri_sub(begin_time, 1,2)) > 9) -> raport
  
  # wyliczamy średni czas odwiedzin każego dnia
  raport %>% 
    group_by(date)%>%
    summarise(srednia = mean(diff_time)) -> daily_visits
  
  # formatujemy datę
  daily_visits$date <- as.Date(daily_visits$date, "%Y-%m-%d")
  
  # dodajemy 0 dla dni, których nie było w ramce
  date_df <- data.frame(date=seq.Date(as.Date("2013-01-01", "%Y-%m-%d"), as.Date("2013-12-31", "%Y-%m-%d"), by="day"))
  date_df %>% left_join(daily_visits) -> daily_visits
  daily_visits$srednia[is.na(daily_visits$srednia)] = 0
  
  # dekompozycja szeregu
  tsDF <- ts(daily_visits$srednia, frequency=7)
  ds <- stl(tsDF,s.window = 'periodic')
  tmp <- as.data.frame(ds$time.series)
  tmp$data = tmp$seasonal + tmp$trend + tmp$remainder
  if(k==1) {ramka <- tmp}
  else{
    colnames(tmp) <- paste0(colnames(tmp), as.character(k))
    ramka <- cbind(ramka, tmp)
  }
}


ramka$time <- seq(from = 1,to = 53,length.out = 365)
head(ramka)

# zbijamy interesujące nas kolumny do jednej ramki, aby mieć dane pogrupowane
# ze względu na stację. 
ramka_melted_trend <- melt(ramka[,c("trend", "trend2", "trend3", "time")], id.vars="time")
head(ramka_melted_trend)


# wykres trendu
cairo_pdf(file = file.path(path, "Plakat", "trend2_czas_laczny_abababa.pdf"), width = 9, height =5)
print(ggplot(data = ramka_melted_trend, aes(time, value, col=variable)) + 
        geom_line() + 
        scale_x_continuous(breaks = seq(1,53,length.out = 13),
                           labels = as.character(format(seq.Date(as.Date("2013-01-01", "%Y-%m-%d"), as.Date("2014-01-01", "%Y-%m-%d"), by="month"), "%d %b"))
        )+
        labs(title ="Trend w średnim czasie odwiedzin dla wybranych stacji",x = "",  y = "Średni czas odwiedzin")+
        scale_color_discrete(name = "Stacja", labels = exs) +
        geom_smooth() + 
        theme(plot.title = element_text(size = 20))+
        theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14)) +
        theme(legend.title=element_text(size=12), legend.text=element_text(size=12)) +
        theme(legend.background = element_rect(colour = alpha('black', .5))) +
        theme(panel.border=element_rect(fill = NA, colour=alpha('black', .5),size=0.5))
        
)
dev.off()

