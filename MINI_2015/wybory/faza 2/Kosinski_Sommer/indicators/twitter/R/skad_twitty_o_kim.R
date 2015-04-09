source("indicators\\twitter\\R\\wczytanie_calej_tabeli.R")

twitty <- cala_tabela$text
skad <- as.character(cala_tabela$statusSource) %>%
   stri_match_all_regex("\\\\>(.*?)</a>$") %>%
   unlist()

skad <- skad[as.logical((1:length(skad)+1) %% 2)]

###############################################################
# wykresy:
###############################################################

wykres <- function(wyrazenie){
   kiedy <- data.frame(table(skad[stri_detect_regex(twitty, wyrazenie)])) %>% 
      filter(!(Freq %in% 0:3))
   
   ggplot(kiedy, aes(x=Var1, y=Freq, group=1))+
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

pdf("indicators\\twitter\\wykresy\\\\skad_twitty.pdf", width=7, height=13)
grid.newpage() 
print(wykres("komorows"), vp=viewport(x=0.25, y = 0.8, width=0.5, height=0.33))
print(wykres("dud"), vp=viewport(x=0.75, y = 0.8, width=0.5, height=0.33))
print(wykres("og[oó]rek"), vp=viewport(x=0.25, y = 0.5, width=0.5, height=0.33))
print(wykres("korwin"), vp=viewport(x=0.75, y = 0.5, width=0.5, height=0.33))
print(wykres("kukiz"), vp=viewport(x=0.5, y = 0.2, width=0.5, height=0.33))
dev.off()



