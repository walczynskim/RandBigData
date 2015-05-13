library(PogromcyDanych)
library(shiny)
library(dplyr)

shinyServer(function(input, output) {

   auta<-PogromcyDanych::auta2012

   output$tabela <- renderTable({

      wynik<-filter(auta,Rok.produkcji<=input$max_rok_produkcji,
         Rok.produkcji>=input$min_rok_produkcji,
         Przebieg.w.km<=input$przebieg,
         Cena.w.PLN<=input$cena[2],
         Cena.w.PLN>=input$cena[1],
         KM<=input$moc[2],
         KM>=input$moc[1],
         Pojemnosc.skokowa<=input$pojemnosc_skokowa[2],
         Pojemnosc.skokowa>=input$pojemnosc_skokowa[1])

      if(input$marka!=""){
         wynik<-filter(wynik, Marka %in% input$marka)
      }

      if(input$model!=""){
         wynik<-filter(wynik, Model %in% input$model)
      }

      if(!is.null(input$paliwo)){
         wynik<-filter(wynik,Rodzaj.paliwa %in% input$paliwo)
      }

      if(input$kraj_pochodzenia!=""){
         wynik<-filter(wynik,wynik$Kraj.pochodzenia %in% input$kraj_pochodzenia)
      }

      if(!is.null(input$liczba_drzwi)){
         wynik<-filter(wynik,wynik$Liczba.drzwi %in% input$liczba_drzwi)
      }

      if(!is.null(input$skrzynia_biegow)){
         wynik<-filter(wynik,wynik$Skrzynia.biegow %in% input$skrzynia_biegow)
      }

      if(input$kolor!=""){
         wynik<-filter(wynik,wynik$Kolor %in% input$kolor)
      }

      if(nrow(wynik)>50){
         wynik<-wynik[1:50,c("Cena.w.PLN","Marka","Model","Rok.produkcji",
           "Przebieg.w.km","Rodzaj.paliwa","KM","kW","Pojemnosc.skokowa",
           "Kraj.pochodzenia","Liczba.drzwi","Skrzynia.biegow","Kolor")]
      }
      else {
         wynik<-wynik[,c("Cena.w.PLN","Marka","Model","Rok.produkcji",
           "Przebieg.w.km","Rodzaj.paliwa","KM","kW","Pojemnosc.skokowa",
           "Kraj.pochodzenia","Liczba.drzwi","Skrzynia.biegow","Kolor")]
      }

    })

})
