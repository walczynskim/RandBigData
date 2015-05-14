library(shiny)
library(PogromcyDanych)
library(dplyr)

shinyServer(function(input, output) {
  
  dane <- PogromcyDanych::auta2012
  dane <- cbind(dane, Cena.w.PLN.brutto = ifelse(dane$Brutto.netto == "brutto", dane$Cena.w.PLN, floor(1.23*dane$Cena.w.PLN)))

    output$tabela <- renderTable({
      dane2 <- filter(dane, Cena.w.PLN.brutto >= input$cena[1]*1000, 
                      Cena.w.PLN.brutto <= input$cena[2]*1000, 
                      Przebieg.w.km >= input$przebieg[1]*1000, 
                      Przebieg.w.km <= input$przebieg[2]*1000, 
                      Rok.produkcji >= input$rok_produkcji[1], 
                      Rok.produkcji <= input$rok_produkcji[2], 
                      Pojemnosc.skokowa >= input$pojemnosc[1], 
                      Pojemnosc.skokowa <= input$pojemnosc[2], 
                      KM >= input$kon[1], 
                      KM <= input$kon[2])
      if(input$waluta != "dowolna"){dane2 <- filter(dane2, Waluta == input$waluta)}
      if(input$marka != "dowolna"){dane2 <- filter(dane2, Marka == input$marka)}
      if(input$model != "dowolny"){dane2 <- filter(dane2, Model == input$model)}
      if(input$skrzynia != "dowolna"){dane2 <- filter(dane2, Skrzynia.biegow == input$skrzynia)}
      if(input$drzwi != "dowolna"){dane2 <- filter(dane2, Liczba.drzwi == input$drzwi)}
      
      if(nrow(dane2) > 70){
        dane2 <- dane2[sample(1:nrow(dane2), 70), c("Marka", "Model", "KM", "Przebieg.w.km", "Rok.produkcji", "Cena.w.PLN.brutto")]
      }
      else{
        dane2 <- dane2[, c("Marka", "Model", "KM", "Przebieg.w.km", "Rok.produkcji", "Cena.w.PLN.brutto")]
        
      }
      dane2
    })


  
  
  
  output$histogram <- renderPlot({
    
    dane2 <- filter(dane, Cena.w.PLN.brutto >= input$cena[1]*1000, 
                    Cena.w.PLN.brutto <= input$cena[2]*1000, 
                    Przebieg.w.km >= input$przebieg[1]*1000, 
                    Przebieg.w.km <= input$przebieg[2]*1000, 
                    Rok.produkcji >= input$rok_produkcji[1], 
                    Rok.produkcji <= input$rok_produkcji[2], 
                    Pojemnosc.skokowa >= input$pojemnosc[1], 
                    Pojemnosc.skokowa <= input$pojemnosc[2], 
                    KM >= input$kon[1], 
                    KM <= input$kon[2])
    if(input$waluta != "dowolna"){dane2 <- filter(dane2, Waluta == input$waluta)}
    if(input$marka != "dowolna"){dane2 <- filter(dane2, Marka == input$marka)}
    if(input$model != "dowolny"){dane2 <- filter(dane2, Model == input$model)}
    if(input$skrzynia != "dowolna"){dane2 <- filter(dane2, Skrzynia.biegow == input$skrzynia)}
    if(input$drzwi != "dowolna"){dane2 <- filter(dane2, Liczba.drzwi == input$drzwi)}
    
    # histogram cen brutto aut w PLN
    hist(dane2$Cena.w.PLN.brutto, col = 'darkgray', border = 'white', xlab = "cena brutta aut (PLN)", 
         ylab = "", main = "Histogram cen brutto wybranych aut w PLN")
    
  })
  
})
