
library(ggplot2)
library(stringi)
library(DT)
library(dplyr)
library(grupa0)



#przy pracy na Windows'ie trzeba kodowanie zmienic
#sciezka_eksponat$dzien_tyg <-suppressWarnings(stri_encode(sciezka_eksponat$dzien_tyg,from="UTF-8",to="Windows 1250"))


shinyServer(function(input, output, session) {
   
   
   output$naj_eksponat <- renderPlot({
      # ramka - sciezka_eksponat
      wykres<-wykres_najdluzsza_eksponat(in_rok=input$e_daty_rok,in_miesiac = input$e_daty_miesiac,in_dzien = input$e_daty_dzien)
      wykres[[1]]
   })
   
   output$naj_eksponat2 <- renderDataTable({
      # ramka - sciezka_eksponat
      wykres<-wykres_najdluzsza_eksponat(in_rok=input$e_daty_rok,in_miesiac = input$e_daty_miesiac,in_dzien = input$e_daty_dzien)
      wykres[[2]]
   }, options = list(dom="tp"))


   output$naj_czas <- renderPlot({
      # ramka - maxczas, maxcza1
      
      if (input$e_daty_dzien=="maks"){input$c_daty_dzien=NA}
      wykres_max_czas(podaj_rok = input$c_daty_rok,podaj_miesiac = input$c_daty_miesiac,podaj_dzien = input$c_daty_dzien)
      
   })
   
   output$typ <- renderPlot({
      # ramka - typowa_sciezka
      typowa.mapa(input$t_daty_rok,input$t_daty_miesiac,input$t_daty_dzien)
   })
   
   output$typ2 <- renderDataTable({
      # ramka - typowa_sciezka
      typowa.pop(input$t_daty_rok,input$t_daty_miesiac,input$t_daty_dzien,ile=5,kraniec=input$t_zmienna)
   }, options = list(dom="tp"))


   output$rozkl <- renderPlot({
      # ramka - rozklady
      
     rozklad(input$jaki_wybor)
      
   })
   
   output$wcloud <- renderPlot({
      
      wordcloud_urz(in_miesiac = input$w_daty_miesiac,in_rok = input$w_daty_rok)
      
   })
   

   
})