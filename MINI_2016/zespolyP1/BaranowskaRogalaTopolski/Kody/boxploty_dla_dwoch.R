library(dplyr)
library(stringi)
library(ggthemes)
library(RColorBrewer)
################################
# Zadaniem skryptu jest wygenerowanie wykresu pod tytułem 
# 'Rozkłady czasów użytkowania dla stacji 'Klonowanie' z uwzględnieniem podziału na:'.
# Użyta została funkcja geom_boxplot() z pakietu ggplot2, oraz funkcja multiplot,
# znaleziona na stronie http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/,
# oraz zmodyfikowana do naszych potrzeb.
################################


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, titlesize = 12) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout)+1, ncol(layout), heights = unit(c(0.7, 4), "null"))))
    grid.text("Rozkłady czasów użytkowania dla stacji 'Klonowanie' \n z uwzględnieniem podziału na:", 
              vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2),
              gp = gpar(fontsize = titlesize))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row+1,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# wczytywanie pliku
exs <-"cnk46a"
path <- file.path("C:", "Users", "Bartek.bartek-pc", "Documents", "RandBigData", "ScrappingTextMining")
raport <- read.csv(file.path(path, "Przetworzone","przetworzone_cnk46a.csv"))

# modyfikacja ramki danych, aby interesujące nas dane miały swoje oddzielne kolumny
raport %>%
  filter(as.numeric(stri_sub(begin_time, 1,2))<= 19, as.numeric(stri_sub(begin_time, 1,2)) > 9) -> raport
raport$weekday <- strftime( raport$date, "%u")
raport$month <- strftime( raport$date, "%m")

# wykresy
getPalette = colorRampPalette(brewer.pal(9, "Spectral"))
t01 <- ggplot(raport, aes(x= weekday, y= diff_time, fill = weekday)) +  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
             shape=18, size=3,show.legend = FALSE) +
  ylim(0,200) + labs(title = "Tygodnie", x = "Dzień tygodnia", y = "Czas użytkowania") + 
  scale_x_discrete(labels = c("pon", "wt", "śr", "czw", "pt", "sob", "nie")) + 
  theme(legend.position="none")+
  theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14)) +
  theme(legend.title=element_text(size=12), legend.text=element_text(size=12)) +
  theme(legend.background = element_rect(colour = alpha('black', .5))) +
  theme(panel.border=element_rect(fill = NA, colour=alpha('black', .5),size=0.5)) +
  scale_fill_manual(values = getPalette(7))
  
getPalette2 = colorRampPalette(brewer.pal(9, "RdBu"))
  
t02 <- ggplot( raport, aes(x= month, y= diff_time, fill = month)) +  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = FALSE) +
  ylim(0,200)  + labs(title = "Miesiące", x = "Miesiąc", y = "") + 
  theme(legend.position="none")+
  theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14)) +
  theme(legend.title=element_text(size=12), legend.text=element_text(size=12)) +
  theme(legend.background = element_rect(colour = alpha('black', .5))) +
  theme(panel.border=element_rect(fill = NA, colour=alpha('black', .5),size=0.5))+
  scale_fill_manual(values = getPalette2(12))

cairo_pdf(file = file.path(path, "Plakat", paste0("cnk46a_ababa", ".pdf")), width = 9, height =5)
multiplot(t01, t02, cols=2, titlesize = 20)
dev.off()
  