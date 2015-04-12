
source("indicators\\twitter\\R\\wczytanie_calej_tabeli.R")

###############################################################
# popularnosc kandydata w ciagu dnia
###############################################################

twitty <- cala_tabela$text
dzien <- as.character(cala_tabela$created)

ktore_dobre <- stri_detect_regex(dzien, "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}")
dzien <- dzien[ktore_dobre]
twitty <- twitty[ktore_dobre]

sekundy <- strftime(dzien, "%s")
o <- order(sekundy)

posort_dzien <- dzien[o]
posort_twitty <- twitty[o]

###############################################################
# wykresy:
###############################################################

wykres <- function(wyrazenie){
   kiedy <- data.frame(table(posort_dzien[stri_detect_regex(posort_twitty, wyrazenie)]))
   kiedy$dzien <- strftime(kiedy$Var1, "%F")
   
   kom <- data.frame(table(kiedy$dzien))
   
   ggplot(kom, aes(x=Var1, y=Freq, group=1))+
      geom_point()+
      geom_line()+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(limits = c(0, NA))+
      ggtitle(wyrazenie)
}

# wykres("komorows")
# wykres("dud")
# wykres("og[oó]rek")
# wykres("korwin")
# wykres("kukiz")

pdf("indicators\\twitter\\wykresy\\kiedy_twitty.pdf", width=7, height=13)
grid.newpage() 
print(wykres("komorows"), vp=viewport(x=0.25, y = 0.8, width=0.5, height=0.3))
print(wykres("dud"), vp=viewport(x=0.75, y = 0.8, width=0.5, height=0.3))
print(wykres("og[oó]rek"), vp=viewport(x=0.25, y = 0.5, width=0.5, height=0.3))
print(wykres("korwin"), vp=viewport(x=0.75, y = 0.5, width=0.5, height=0.3))
print(wykres("kukiz"), vp=viewport(x=0.5, y = 0.2, width=0.5, height=0.3))
dev.off()
