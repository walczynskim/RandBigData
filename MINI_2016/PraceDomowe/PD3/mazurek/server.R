
library(ggplot2)
library(stringi)
library(DT)
library(dplyr)

sciezka<-getwd()
sciezka <- "../../../materialy/"
load(file.path(sciezka,"wynikiDF.rda"))

wynikiDF<-as.data.frame.matrix(wynikiDF)

#przy pracy na Windows'ie trzeba kodowanie zmienic
#wynikiDF$oferta<-suppressWarnings(stri_encode(wynikiDF$oferta,"UTF-8","CP1250"))
#wynikiDF$lokacja<-suppressWarnings(stri_encode(wynikiDF$lokacja,"UTF-8","CP1250"))
#wynikiDF$firma<-suppressWarnings(stri_encode(wynikiDF$firma,"UTF-8","CP1250"))
#wynikiDF$stanowisko<-suppressWarnings(stri_encode(wynikiDF$stanowisko,"UTF-8","CP1250"))

sport <- stri_detect_fixed(wynikiDF$oferta, " sport")
multisport <- stri_detect_fixed(wynikiDF$oferta, "multisport")

wynikiDF$woj <- gsub(".*, ", "", wynikiDF$lokacja)
wynikiDF$multisport <- multisport

shinyServer(function(input, output, session) {
  
  output$sport = renderDataTable({
     if (input$jaki_wybor=="firmy"){
        wynikiDF[sport, 2:6] %>% filter(firma==input$firma_sport)
     } else if (input$jaki_wybor=="daty"){
        wynikiDF[sport, 2:6] %>% filter(data == input$data)
     } else {
        wynikiDF[sport, 2:6] %>% filter(data == input$data,firma==input$firma_sport)
     }
  }, options = list(dom="tp"))
  
  output$multisport = renderDataTable({
     if (input$jaki_wybor=="firmy"){
        wynikiDF[multisport, 2:6] %>% filter(firma==input$firma_multisport) 
     } else if (input$jaki_wybor=="daty"){
        wynikiDF[multisport, 2:6] %>% filter(data == input$data) 
     } else {
        wynikiDF[multisport, 2:6] %>% filter(data == input$data,firma==input$firma_multisport) 
     }
  }, options = list(dom="tp"))
  
  output$woj <- renderPlot({
    wynikiDF[multisport, ] %>% group_by(woj) %>%
      summarise(ile = n()) %>%
      arrange(ile) -> dane
    reorder(dane$woj, -dane$ile, mean) -> dane$woj
    
    ggplot(dane, aes(factor(woj), y = ile)) + 
      geom_bar(stat = "identity",fill="lightseagreen") + 
      coord_flip()+
       labs(y="Liczba ogłoszeń",x="Województwo",title="Liczba ogłoszeń w danej lokalizacji")
    
  })
  
  output$stan <- renderPlot({
    wynikiDF %>% group_by(stanowisko) %>%
      summarise(ile = sum(multisport)/n()) %>%
      arrange(ile) -> dane
    reorder(dane$stanowisko, -dane$ile, mean) -> dane$stanowisko
    
    ggplot(dane, aes(factor(stanowisko), y = ile)) + 
      geom_bar(stat = "identity",fill="lightseagreen") + 
      coord_flip()+
       labs(y="Procent ogłoszeń",x="Stanowisko",title="Procent ogłoszeń dla danego stanowiska zawierających pakiet multisport")
    
  })
  
  output$woj_s <- renderPlot({
     wynikiDF[sport, ] %>% group_by(woj) %>%
        summarise(ile = n()) %>%
        arrange(ile) -> dane
     reorder(dane$woj, -dane$ile, mean) -> dane$woj
     
     ggplot(dane, aes(factor(woj), y = ile)) + 
        geom_bar(stat = "identity",fill="lightseagreen") + 
        coord_flip()+
        labs(y="Liczba ogłoszeń",x="Województwo (ew. państwo)",title="Liczba ogłoszeń w danej lokalizacji")
     
  })
  
  output$stan_s <- renderPlot({
     wynikiDF %>% group_by(stanowisko) %>%
        summarise(ile = sum(sport)/n()) %>%
        arrange(ile) -> dane
     reorder(dane$stanowisko, -dane$ile, mean) -> dane$stanowisko
     
     ggplot(dane, aes(factor(stanowisko), y = ile)) + 
        geom_bar(stat = "identity",fill="lightseagreen") + 
        coord_flip()+
        labs(y="Procent ogłoszeń",x="Stanowisko",title="Procent ogłoszeń dla danego stanowiska zawierających pakiet sportowy")
     
  })
  
})