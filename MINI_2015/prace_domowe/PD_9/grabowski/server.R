library(shiny)
library(dplyr)
library(PogromcyDanych)
carsDB <- auta2012
shinyServer(function(input, output) {
  output$output1 <- renderTable({
    carsFiltered <- filter(carsDB, Cena.w.PLN <= input$cena[2] & Cena.w.PLN >= input$cena[1]) %>%
      filter(Rok.produkcji >= input$minRok & Rok.produkcji<=input$maxRok) %>%
      filter(Przebieg.w.km <= input$przebieg) %>%
      filter(KM<=input$moc[2] & KM>=input$moc[1]) 
    if(input$marka != "") {carsFiltered <- filter(carsFiltered, Marka %in% input$marka)}
    if(input$kolor != ""){ carsFiltered <- filter(carsFiltered,carsFiltered$Kolor %in% input$kolor)}
    if(input$model != "") {carsFiltered <- filter(carsFiltered, Model %in% input$model)}
    if(!is.null(input$paliwo)){carsFiltered <- filter(carsFiltered,Rodzaj.paliwa %in% input$paliwo)}
    if(input$krajPochodzenia != ""){carsFiltered <-filter(carsFiltered,
                                                        carsFiltered$Kraj.pochodzenia %in% input$krajPochodzenia)}
    if(input$sortuj == 'rosnaco') {carsFiltered <- arrange(carsFiltered,Cena.w.PLN) } else 
      {carsFiltered <- arrange(carsFiltered, desc(Cena.w.PLN))}
    carsFiltered <- carsFiltered[1:100,c("Cena.w.PLN","Marka","Model","Rok.produkcji",
                    "Przebieg.w.km","Rodzaj.paliwa","KM","Kraj.pochodzenia",
                    "Liczba.drzwi","Kolor")]
  })
})
