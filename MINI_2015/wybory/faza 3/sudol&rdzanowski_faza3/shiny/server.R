library(shiny)
library(lazyeval)
library(wsk)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) {
  # wczytuje dane
  dane <- wsk::wybory2015

  # tutaj tworze tabele wskaznikow dla wybranych portali i kandydatow
  output$tabela <- renderTable({
    
    # filtrowanie portali
    portale <- c("gazeta", "newsweek", "onet", "onet_wybory", "tvn", "tvn_wybory", 
                 "tvp", "tweeter_kandydat", "tweeter_nasluch", "wp")
    names(portale) <- c("gazeta.pl", "newsweek.pl", "onet.pl", "onet.pl/wybory", 
                 "tvn24.pl", "tvn24.pl/wybory", "tvpinfo.pl", "twitter_kandydat", 
                 "twitter_nasluch", "wp.pl")
    wybrane_portale <- portale[names(portale) %in% input$portal]
    dane2 <- filter(dane, portal %in% wybrane_portale)
    
    # filtrowanie kandydatow (oraz sentymentu)
    kandydat <- c("komorowski", "duda", "jarubas", "ogorek", "palikot", "korwin", "kukiz")
    names(kandydat) <- c("B. Komorowski", "A. Duda", "A. Jarubas", "M. Ogorek", "J. Palikot", 
                         "J. Korwin-Mikke", "P. Kukiz")
    wybrani_kandydaci <- kandydat[names(kandydat) %in% input$kandydat]
    dane2 <- dane2[,c(wybrani_kandydaci, input$sentyment, "l_retweet", "l_polubien", "data")]
    
    wskazniki <- c("pstwo_widocznosci", "pstwo_poz", "pstwo_neg", "pstwo_neu", 
                   "szansa_poz", "szansa_neg", "szansa_neu", "poz_do_neg")
    names(wskazniki) <- c("p-stwo widocznosci kandydata", 
                          "p-stwo pozytywnej widocznosci kandydata", 
                          "p-stwo negatywnej widocznosci kandydata", 
                          "p-stwo neutralnej widocznosci kandydata", 
                          "szansa pozytywnej widocznosci kandydata", 
                          "szansa negatywnej widocznosci kandydata", 
                          "szansa neutralnej widocznosci kandydata", 
                          "stosunek tekstow pozytywnych do negatywnych")
    wybrany_wskaznik <- wskazniki[names(wskazniki) == input$wskaznik]
    
    if(length(input$kandydat) == 0 | length(input$portal) == 0){return(NULL)}
    else{      
      ramka_wskaznik <- data.frame(data = character(0), wsk = numeric(0), 
                                   nazwisko = character(0))
      
      for(i in 1:length(input$kandydat)){
        if(wybrany_wskaznik %in% "pstwo_widocznosci"){
          kompiluj <- stri_paste("dane2 %>% group_by(data) %>% summarise(wsk = ", 
                                 wybrany_wskaznik, "(", wybrani_kandydaci[i], 
                                 "))")
        }
        else if(wybrany_wskaznik %in% c("pstwo_poz", "pstwo_neg", "pstwo_neu", 
                                        "szansa_poz", "szansa_neg", "szansa_neu", 
                                        "poz_do_neg")){
          kompiluj <- stri_paste("dane2 %>% group_by(data) %>% summarise(wsk = ", 
                                 wybrany_wskaznik, "(", wybrani_kandydaci[i], 
                                 ", wektor_sentyment = ", input$sentyment, "))")
        }
        dane_wsk <- data.frame(eval(parse(text = kompiluj)))
        dane_wsk <- cbind(dane_wsk, nazwisko = rep(input$kandydat[i], nrow(dane_wsk)))
        ramka_wskaznik <- rbind(ramka_wskaznik, dane_wsk)
      }
      return(ramka_wskaznik)
    }
  })
  
  # tutaj tworze wykres zaleznosci wskaznikow widocznosci kandydata od czasu
  output$wykres1 <- renderPlot({
    # filtrowanie portali
    portale <- c("gazeta", "newsweek", "onet", "onet_wybory", "tvn", "tvn_wybory", 
                 "tvp", "tweeter_kandydat", "tweeter_nasluch", "wp")
    names(portale) <- c("gazeta.pl", "newsweek.pl", "onet.pl", "onet.pl/wybory", 
                        "tvn24.pl", "tvn24.pl/wybory", "tvpinfo.pl", "twitter_kandydat", 
                        "twitter_nasluch", "wp.pl")
    wybrane_portale <- portale[names(portale) %in% input$portal]
    dane2 <- filter(dane, portal %in% wybrane_portale)
    
    # filtrowanie kandydatow (oraz sentymentu)
    kandydat <- c("komorowski", "duda", "jarubas", "ogorek", "palikot", "korwin", "kukiz")
    names(kandydat) <- c("B. Komorowski", "A. Duda", "A. Jarubas", "M. Ogorek", "J. Palikot", 
                         "J. Korwin-Mikke", "P. Kukiz")
    wybrani_kandydaci <- kandydat[names(kandydat) %in% input$kandydat]
    dane2 <- dane2[,c(wybrani_kandydaci, input$sentyment, "l_retweet", "l_polubien", "data")]
    
    wskazniki <- c("pstwo_widocznosci", "pstwo_poz", "pstwo_neg", "pstwo_neu", 
                   "szansa_poz", "szansa_neg", "szansa_neu", "poz_do_neg", 
                   "twitt_poz_neg", "twitt_poz_neg")
    names(wskazniki) <- c("p-stwo widocznosci kandydata", 
                          "p-stwo pozytywnej widocznosci kandydata", 
                          "p-stwo negatywnej widocznosci kandydata", 
                          "p-stwo neutralnej widocznosci kandydata", 
                          "szansa pozytywnej widocznosci kandydata", 
                          "szansa negatywnej widocznosci kandydata", 
                          "szansa neutralnej widocznosci kandydata", 
                          "stosunek tekstow pozytywnych do negatywnych", 
                          "roznica udostepnien twittow pozytywnych i negatywnych", 
                          "roznica polubien twittow pozytywnych i negatywnych")
    wybrany_wskaznik <- wskazniki[names(wskazniki) == input$wskaznik]
    
    if(length(input$kandydat) == 0 | length(input$portal) == 0){return(NULL)}
    else{      
      ramka_wskaznik <- data.frame(data = character(0), wsk = numeric(0), 
                                   nazwisko = character(0))
      
      for(i in 1:length(input$kandydat)){
        if(wybrany_wskaznik %in% "pstwo_widocznosci"){
          kompiluj <- stri_paste("dane2 %>% group_by(data) %>% summarise(wsk = ", 
                                 wybrany_wskaznik, "(", wybrani_kandydaci[i], 
                                 "))")
        }
        else if(wybrany_wskaznik %in% c("pstwo_poz", "pstwo_neg", "pstwo_neu", 
                                        "szansa_poz", "szansa_neg", "szansa_neu", 
                                        "poz_do_neg")){
          kompiluj <- stri_paste("dane2 %>% group_by(data) %>% summarise(wsk = ", 
                                 wybrany_wskaznik, "(", wybrani_kandydaci[i], 
                                 ", wektor_sentyment = ", input$sentyment, "))")
        }
        dane_wsk <- data.frame(eval(parse(text = kompiluj)))
        dane_wsk <- cbind(dane_wsk, nazwisko = rep(input$kandydat[i], nrow(dane_wsk)))
        ramka_wskaznik <- rbind(ramka_wskaznik, dane_wsk)
      }
    }
    
    ile_dni <- nrow(ramka_wskaznik)/length(input$kandydat)
    ramka_wskaznik <- cbind(ramka_wskaznik, dzien = rep(1:ile_dni, length(input$kandydat)))
    
    dni_daty <- as.character(ramka_wskaznik$data[1:ile_dni])
    
    rysunek <- ggplot(data = ramka_wskaznik, aes(x = dzien, y = wsk, 
                            color = nazwisko))+
      geom_point()+
      geom_line(size = 1.5)+
      labs(x = "data", y = "wskaznik")+
      theme(
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)   
      )+
      theme(legend.title = element_text(colour = "black", size = 18))+
      scale_colour_discrete(name = "Kandydat:")+
      theme(legend.text = element_text(size = 14, colour = "black", angle = 0))+
      scale_x_continuous(breaks = seq(1, ile_dni, by = 6), labels = dni_daty[c(TRUE, rep(FALSE,5))])
    if(input$smooth == "nie rysuj"){
      return(rysunek)
    }
    else if(input$smooth == "lm"){
      rysunek <- rysunek+geom_smooth(method = lm, se = FALSE, size = 1.5)
      return(rysunek)
    }
    else if(input$smooth == "loess"){
      rysunek <- rysunek+geom_smooth(method = loess, se = FALSE, size = 1.5)
      return(rysunek)
    }  
    
  })
  
  
  # tutaj tworze wykres zaleznosci wskaznika 'twitt_poz_neg' od czasu
  output$wykresik <- renderPlot({
    # ograniczam sie jedynie do danych z twitter_nasluch
    dane3 <- dane %>% filter(portal == "tweeter_nasluch")
    
    # filtrowanie kandydatow (oraz sentymentu)
    kandydat <- c("komorowski", "duda", "jarubas", "ogorek", "palikot", "korwin", "kukiz")
    names(kandydat) <- c("B. Komorowski", "A. Duda", "A. Jarubas", "M. Ogorek", "J. Palikot", 
                         "J. Korwin-Mikke", "P. Kukiz")
    wybrani_kandydaci <- kandydat[names(kandydat) %in% input$kandydat]
    dane3 <- dane3[,c(wybrani_kandydaci, input$sentyment, "l_retweet", "l_polubien", "data")]
    
    if(length(input$kandydat) == 0 ){return(NULL)}
    else{      
      ramka_wskaznik <- data.frame(data = character(0), wsk = numeric(0), 
                                   nazwisko = character(0), 
                                   rodzaj = character(0))
      
      for(i in 1:length(input$kandydat)){
        kompiluj <- stri_paste("dane3 %>% group_by(data) %>% summarise(wsk = ", 
                                "twitt_poz_neg", "(", wybrani_kandydaci[i], 
                                ", wektor_sentyment = ", input$sentyment, 
                                ", liczba_twitt = l_retweet ))")

        dane_wsk <- data.frame(eval(parse(text = kompiluj)))
        dane_wsk <- cbind(dane_wsk, nazwisko = rep(input$kandydat[i], nrow(dane_wsk)), 
                          rodzaj = rep("udostepnienia", nrow(dane_wsk)))
        ramka_wskaznik <- rbind(ramka_wskaznik, dane_wsk)
        
        
        kompiluj <- stri_paste("dane3 %>% group_by(data) %>% summarise(wsk = ", 
                               "twitt_poz_neg", "(", wybrani_kandydaci[i], 
                               ", wektor_sentyment = ", input$sentyment, 
                               ", liczba_twitt = l_polubien ))")
        
        dane_wsk <- data.frame(eval(parse(text = kompiluj)))
        dane_wsk <- cbind(dane_wsk, nazwisko = rep(input$kandydat[i], nrow(dane_wsk)), 
                          rodzaj = rep("polubienia", nrow(dane_wsk)))
        ramka_wskaznik <- rbind(ramka_wskaznik, dane_wsk)
      }
    }
    
    ile_dni <- nrow(ramka_wskaznik)/(2*length(input$kandydat))
    ramka_wskaznik <- cbind(ramka_wskaznik, dzien = rep(1:ile_dni, 2*length(input$kandydat)))
    
    dni_daty <- as.character(ramka_wskaznik$data[1:ile_dni])
    
    rysunek <- ggplot(data = ramka_wskaznik, aes(x = dzien, y = wsk, 
                                                 colour = nazwisko))+
      geom_point()+
      geom_line(size = 1.5)+
      facet_grid(rodzaj ~ ., scales = "free_y")+
      labs(x = "data", y = "wskaznik")+
      theme(
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)   
      )+
      theme(legend.title = element_text(colour = "black", size = 18))+
      scale_colour_discrete(name = "Kandydat:")+
      theme(legend.text = element_text(size = 14, colour = "black", angle = 0))+
      scale_x_continuous(breaks = seq(1, ile_dni, by = 6), labels = dni_daty[c(TRUE, rep(FALSE,5))])      
      
    if(input$smooth == "nie rysuj"){
      return(rysunek)
    }
    else if(input$smooth == "lm"){
      rysunek <- rysunek+geom_smooth(method = lm, se = FALSE, size = 1.5)
      return(rysunek)
    }
    else if(input$smooth == "loess"){
      rysunek <- rysunek+geom_smooth(method = loess, se = FALSE, size = 1.5)
      return(rysunek)
    }
    
  })

})