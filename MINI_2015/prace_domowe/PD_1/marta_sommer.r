# Porównanie filmów: "50 Twarzy Greya" i "Lot nad Kukułczym Gniazdem":

library("stringi")
library("XML")
library("scales")
library("ggplot2")
library("dplyr")

# "50 Twarzy Greya":

stat <- readHTMLTable("http://www.imdb.com/title/tt2322441/ratings?ref_=tt_ov_rt", 
                      stringsAsFactors = FALSE)
stat2 <- as.data.frame(stat[2])

kobiety2 <- stat2[c(5,8,11,14), c(1,3)]
kobiety2[,2] <- as.numeric(unlist(stri_extract_all_regex(kobiety2[,2], "[0-9.]+")))
colnames(kobiety2) <- c("wiek", "srednia")

mezczyzni2 <- stat2[c(4,7,10,13), c(1,3)]
mezczyzni2[,2] <- as.numeric(unlist(stri_extract_all_regex(mezczyzni2[,2], "[0-9.]+")))
colnames(mezczyzni2) <- c("wiek", "srednia")

# "Lot nad Kukułczym Gniazdem":

stat <- readHTMLTable("http://www.imdb.com/title/tt0073486/ratings?ref_=tt_ov_rt", 
                    stringsAsFactors = FALSE)
stat2<-as.data.frame(stat[2])

kobiety1 <- stat2[c(5,8,11,14), c(1,3)]
kobiety1[,2] <- as.numeric(unlist(stri_extract_all_regex(kobiety1[,2], "[0-9.]+")))
colnames(kobiety1) <- c("wiek", "srednia")

mezczyzni1 <- stat2[c(4,7,10,13), c(1,3)]
mezczyzni1[,2] <- as.numeric(unlist(stri_extract_all_regex(mezczyzni1[,2], "[0-9.]+")))
colnames(mezczyzni1) <- c("wiek", "srednia")

# razem:

film <- c("Lot nad Kukułczym Gniazdem","50 Twarzy Greya")

mezczyzni1$plec <- "m"
mezczyzni2$plec <- "m"
kobiety1$plec <- "k"
kobiety2$plec <- "k"

mezczyzni1$film <- film[1]
mezczyzni2$film <- film[2]
kobiety1$film <- film[1]
kobiety2$film <- film[2]

tabela <- rbind(mezczyzni1, mezczyzni2, kobiety1, kobiety2)

a <- tabela$wiek
a <- stri_replace_all_fixed(a,"Males under ","< ")
a <- stri_replace_all_fixed(a,"Females under ","< ")
a <- stri_replace_all_fixed(a,"Males Aged ","")
a <- stri_replace_all_fixed(a,"Females Aged ","")
a <- stri_replace_all_fixed(a,"45+","> 45")
tabela$wiek <- a

tabela$srednia <- tabela$srednia/10
tabela$wiek <- factor(tabela$wiek, c("< 18", "18-29", "30-44", "> 45"))

# wykres:

ggplot(tabela, aes(x=wiek, y=srednia, fill=plec))+
  geom_bar(stat="identity", position="dodge", color="black")+
  facet_grid(.~film)+
  theme_bw()+
  ggtitle("50 TWARZY GREYA DZIELI KOBIETY I MĘŻCZYZN!\n")+
  theme(axis.text = element_text(colour="grey20",size=16,face="plain",
                                 family="serif"),
        axis.title.x = element_text(family="serif", size=16),
        axis.title.y = element_text(family="serif", size=16),
        plot.title=element_text(size=18,family="serif"),
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(),
        strip.text = element_text(size = 18, family="serif"),
        legend.text = element_text(size = 16, family="serif"),
        legend.title = element_text(size = 16, family = "serif"))+
  scale_y_continuous(labels=percent)+
  xlab("Kategeria wiekowa")+
  ylab("Średnia ocena filmu")+
  scale_fill_manual(values=c("pink", "darkgray"), guide = guide_legend(title = "Płeć"),
                    labels=c("Kobiety", "Mężczyźni"))
  




