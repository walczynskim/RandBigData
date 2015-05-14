
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(PogromcyDanych)
library(dplyr)
library(stringi)
library(ggplot2)

shinyServer(function(input, output,clientData, session) {

  
  observe({
    
    if(length(input$marka)>0){
      tab <- auta2012[which(auta2012$Marka%in%input$marka),]
    } else{
      tab <- auta2012
    }
    
    
    dane <- reactive({
      
      if(length(input$model)>0){     
        tab <- tab %>%
          filter(Model==input$model)
      }
      
      tab <- tab %>%
        filter(Rok.produkcji>=as.character(input$data2))
      
      tab <- tab %>%
        filter(Cena>=input$cena[1] & Cena<=input$cena[2])
      
      tab <- tab %>%
        filter(Przebieg.w.km<=input$przebieg)
      
      tab <- tab %>%
        filter(KM>=input$km)
      
      if(input$kraj !="Obojetnie"){
        
        tab <- tab %>%
          filter(Kraj.pochodzenia %in% input$kraj)
        
      }
      
      if(input$czy_pln){
        
        tab <- tab %>% 
          filter(Waluta=="PLN")
        
      } else{
        tab <- tab %>% 
          filter(Waluta!="PLN")
        
      }
      
      tab <- tab %>%
        filter(Rodzaj.paliwa%in%input$rodzaj_paliwa)
      
      tab  
      
      
    })
    
    s_options <- unique(tab$Model)
    maksimum <- max(tab$Cena)
    minimum <- min(tab$Cena)
    
    rok_minimum <- min(tab$Rok.produkcji)
    
    rok_maksimum <- max(tab$Rok.produkcji)
    
    
    przebieg_maksimum <- max(tab$Przebieg.w.km,na.rm=TRUE)
    
    updateSelectInput(session, "model",
                      choices = s_options,
                      selected = paste0("option-", "-A")
    )
    
    updateSliderInput(session, "cena",min=minimum, max=maksimum,
                      value=c(minimum,maksimum))
    updateSliderInput(session, "data", min=rok_minimum, max=rok_maksimum,
                      value=c(rok_minimum,rok_maksimum))
    updateSliderInput(session, "przebieg",min=0, max=przebieg_maksimum,
                      value=c(przebieg_maksimum))
    
    output$dane_analizowane <- renderTable({
      
     dane()
      
    })
    
    output$info <- renderPrint({
      
      napis1 <- stri_paste("Nazywasz sie ", input$dane,".")
      napis2 <- stri_paste("Masz ",input$wiek, " lata. ")
      napis3 <- stri_paste("Interesuja Cie marki:")
      print(napis1)
      print(napis2)
      print(napis3)
      print(input$marka)
      print("Zapraszam do obejrzenia analizy.")
      
    })
    
    output$wykres <- renderPlot({
      
      
      nowy <- dane() %>%
        group_by(Kraj.aktualnej.rejestracji, Marka) %>%
        summarise(a=n())
      if(nrow(dane())){
        ggplot(nowy, aes(x=Kraj.aktualnej.rejestracji, y=a, fill=Marka))+geom_bar(position="dodge",stat="identity")
      }
      
      
    })
    
  })


})
