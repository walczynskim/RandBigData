library(shiny)
library(PogromcyDanych)
library(dplyr)

shinyServer(function(input, output) {
             
    output$Tabela <- renderTable({
      
      auta2012 %>% 
        filter(Marka %in% input$marka) %>%
        filter(Model %in% input$model) %>%
        filter(Skrzynia.biegow %in% input$skrzynia_biegow)%>%
        filter(Rok.produkcji >= as.numeric(input$rok)) %>%
        filter(Przebieg.w.km <= input$przebieg) %>%
        filter(Cena.w.PLN >= input$cena[1]) %>%
        filter(Cena.w.PLN <= input$cena[2]) %>%
        filter(Rodzaj.paliwa %in% input$paliwo) %>%
        filter(Kolor %in% input$kolor) %>%
        filter(Liczba.drzwi %in% input$drzwi)
                  
  })
})
