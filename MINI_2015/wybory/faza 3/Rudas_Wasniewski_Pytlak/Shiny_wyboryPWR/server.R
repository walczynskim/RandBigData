library(shiny)
library(pakietWyboryPRW)
# library(stringi)
# library(ggplot2)
# library(tidyr)
# library(dplyr)


shinyServer(function(input, output) {
  
############ wskazniki liniowe

   output$wykres_ile_dziennie <- renderPlot({
      if (length(input$kandydaci_dzien)==0) return(NULL)
      wykres_ile_dziennie(dane_artykuly,co=input$co_dzien,
                          zrodlo1=input$zrodlo_dzien,
                        od_kiedy=as.Date(input$od_do_dzien[1], "%d-%m-%Y"),
                        do_kiedy=as.Date(input$od_do_dzien[2], "%d-%m-%Y"),
                        ktorzykandydaci=input$kandydaci_dzien
                        )
   })
   
   output$wykresilezliczenprzezDni<-renderPlot({
      if (length(input$kandydaci_dzien)==0) return(NULL)
      wykresilezliczenprzezDni(dane = dane_artykuly,
                               zrodlo = input$zrodlo_dzien,
                               co = input$co_dzien,
                               od_ktorego = input$od_do_dzien[1],
                               do_ktorego = input$od_do_dzien[2],
                               ktorzykandydaci = input$kandydaci_dzien,
                               coile=3
      )
   })
   


   #wykres liczby serii:
   output$wykresLiczbySerii <- renderPlot({
      if (length(input$kandydaci_dzien)==0) return(NULL)
      wykresLiczbySerii(dane_artykuly, 
                        zrodlo=input$zrodlo_dzien,
                        co=input$co_dzien, 
                        od_ktorego = input$od_do_dzien[1],
                        do_ktorego = input$od_do_dzien[2],
                        liczbaserii=as.numeric(input$liczbaserii_dzien),
                        coile=3,
                        ktorzykandydaci=input$kandydaci_dzien)
   })
   
   #wykres maksymalnej serii:
   output$wykresMaksymalnejSerii <- renderPlot({
      if (length(input$kandydaci_dzien)==0) return(NULL)
      wykresMaksymalnejSerii(dane_artykuly,
                             co=input$co_dzien, 
                             zrodlo=input$zrodlo_dzien,
                             od_ktorego = input$od_do_dzien[1],
                             do_ktorego = input$od_do_dzien[2],
                             coile=3,
                             ktorzykandydaci=input$kandydaci_dzien)
   })
   
   #wykres przez ile Dni
   output$wykresprzezileDni<-renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      wykresprzezileDni(dane_artykuly,
                        zrodlo=input$zrodlo_dzien,
                        co=input$co_dzien,
                        od_ktorego=input$od_do_dzien[1],
                        do_ktorego=input$od_do_dzien[2],
                        coile=3,
                        ktorzykandydaci=input$kandydaci_dzien
      )
   })
   
   output$podsumowanie <- renderText({
      print(input$kandydaci_ofic)
      dim(dane_artykuly)
   })
   
 
############# wskazniki nieliniowe 
   
   #wykres liczby serii2:
   output$wykresLiczbySerii2 <- renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      wykresLiczbySerii2(dane_artykuly,co=input$co_ilosc, 
                        zrodlo=input$zrodlo_ilosc,
                        od_ktorego=input$od_do_ilosc[1],
                        do_ktorego=input$od_do_ilosc[2],
                        liczbaserii=as.numeric(input$liczbaserii_ilosc),
                        ktorzykandydaci=input$kandydaci_ilosc)
   })
   
   # wykres maksymalnej serii 2
   output$wykresMaksymalnejSerii2 <- renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      wykresMaksymalnejSerii2(dane_artykuly,co=input$co_ilosc, 
                             zrodlo=input$zrodlo_ilosc,
                             od_ktorego=input$od_do_ilosc[1],
                             do_ktorego=input$od_do_ilosc[2],
                             ktorzykandydaci=input$kandydaci_ilosc)
   })
   
   # wzkres rangi
   output$wykresRangi<-renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      wykresRangi(dane=dane_artykuly,
                  zrodlo=input$zrodlo_ilosc,
                  co=input$co_ilosc,
                  od_ktorego=input$od_do_ilosc[1],
                  do_ktorego=input$od_do_ilosc[2],
                  ktorzykandydaci=input$kandydaci_ilosc,
                  funkcja="median"
      )
   })
   
   # wykres zliczen przez
   output$wykresprzezileDni2<-renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      wykresprzezileDni2(dane=dane_artykuly,
                         zrodlo=input$zrodlo_ilosc,
                         co=input$co_ilosc,
                         od_ktorego=input$od_do_ilosc[1],
                         do_ktorego=input$od_do_ilosc[2],
                         ktorzykandydaci=input$kandydaci_ilosc
      )
   })
   
   #wykres pory dnia 
   output$wykres_pora_dnia<-renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      pakietWyboryPRW::wykres_pora_dnia(dane=dane_artykuly,
                                        zrodlo1=input$zrodlo_ilosc,
                                        od_ktorego=input$od_do_ilosc[1],
                                        do_ktorego=input$od_do_ilosc[2],
                                        co=input$co_ilosc,
                                        ktorzykandydaci=input$kandydaci_ilosc
      )
   })
   
   # box plot dla czestosci
   output$Boxplot_czestotliwosci<-renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      Boxplot_czestotliwosci(dane=dane_artykuly,
                             zrodlo=input$zrodlo_ilosc,
                             co=input$co_ilosc,
                             od_ktorego=input$od_do_ilosc[1],
                             do_ktorego=input$od_do_ilosc[2],
                             ktorzykandydaci=input$kandydaci_ilosc
      )
   })
   
   # box plot zliczen przez liczbe dni 
   output$wykresilezliczenprzezDni2<-renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      wykresilezliczenprzezDni2(dane=dane_artykuly,
                          zrodlo=input$zrodlo_ilosc,
                          co=input$co_ilosc,
                          od_ktorego=input$od_do_ilosc[1],
                          do_ktorego=input$od_do_ilosc[2],
                          ktorzykandydaci=input$kandydaci_ilosc
                          )
   })
   
   # box plot dla komentarzy
   output$wykres_komentarzy<-renderPlot({
      if (length(input$kandydaci_ilosc)==0) return(NULL)
      wykres_komentarzy(dane=dane_artykuly,
                        Zrodlo=input$zrodlo_ilosc,
                        co=input$co_ilosc,
                        od_ktorego=input$od_do_ilosc[1],
                        do_ktorego=input$od_do_ilosc[2],
                        ktorzykandydaci=input$kandydaci_ilosc
      )
   })
   

######### wskazniki dla oficjanych tweeterow kandydatow
   
   # ilosc oficjalnych tweetow na dzien
   output$wykres_ilosc_ofic <- renderPlot({
      if (length(input$kandydaci_ofic)==0) return(NULL)
      wykres_ilosc_ofic(data_od=input$od_do_ofic[1], 
                        data_do=input$od_do_ofic[2], 
                        kandydaci=input$kandydaci_ofic)
   })
   
   # ilosc ulubionych tweetow na dzien
   output$wykres_ulubione_ofic <- renderPlot({
      if (length(input$kandydaci_ofic)==0) return(NULL)
      wykres_ulubione_ofic(data_od=input$od_do_ofic[1], 
                           data_do=input$od_do_ofic[2], 
                           kandydaci=input$kandydaci_ofic)
   })
   
   output$wykres_retweety_ofic <- renderPlot({
      if (length(input$kandydaci_ofic)==0) return(NULL)
      wykres_retweety_ofic(data_od=input$od_do_ofic[1], 
                           data_do=input$od_do_ofic[2], 
                           kandydaci=input$kandydaci_ofic)
   })
   
   output$wykres_wydzwiek_ofic <- renderPlot({
      if (length(input$kandydaci_ofic)==0) return(NULL)
      wykres_wydzwiek_ofic(data_od=input$od_do_ofic[1], 
                           data_do=input$od_do_ofic[2], 
                           kandydaci=input$kandydaci_ofic)
   })
   
 #### wskazniki dla nasluchu twittera
   
   # liczba tweetow na dzien 
   output$wykres_ilosc_nasluch <- renderPlot({
      if (length(input$kandydaci_nasluch)==0) return(NULL)
      wykres_ilosc_nasluch(data_od=input$od_do_nasluch[1], 
                           data_do=input$od_do_nasluch[2], 
                           kandydaci=input$kandydaci_nasluch)
   })
   
   # wydzwiek na dzien
   output$wykres_wydzwiek_nasluch <- renderPlot({
      if (length(input$kandydaci_nasluch)==0) return(NULL)
      wykres_wydzwiek_nasluch(data_od=input$od_do_nasluch[1], 
                              data_do=input$od_do_nasluch[2], 
                              kandydaci=input$kandydaci_nasluch)
   })
})