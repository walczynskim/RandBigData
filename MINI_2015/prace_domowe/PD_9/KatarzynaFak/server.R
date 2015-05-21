library(shiny)
library(PogromcyDanych)
library(stringi)

shinyServer(function(input, output) {
   
   # bez wybrania modelu wyposazenie sie nie wyszuka.
   daneWyposazenie <- reactive({
      if(nchar(input$wyposazenie)>1)
            auta2012 %>% filter( grepl( paste0(strsplit(paste0("[",input$wyposazenie,"]"), " ")[[1]], collapse="]["), Wyposazenie.dodatkowe)) else
               auta2012
   })
   
   daneModel <- reactive({
      if(all(input$marka==levels(daneWyposazenie()$Marka)))
         daneWyposazenie() else
      daneWyposazenie() %>%
         filter(Marka%in%input$marka) %>%
         droplevels()
   })
   
   daneModelMarka <- reactive({
      if(all(input$model==levels(auta2012$Model)))
         daneModel() else
      daneModel() %>%
         filter(Model%in%input$model) %>% 
         droplevels()
   })
   
   output$model <- renderUI({
      selectInput("model","Model", levels(daneModel()$Model), multiple=TRUE)
   })

   output$cena <- renderUI({
         sliderInput("cena","Cena", 
                     min(daneModelMarka()$Cena.w.PLN),
                     max(daneModelMarka()$Cena.w.PLN),
                     value=range(daneModelMarka()$Cena.w.PLN))
   })
   
   output$rok <- renderUI({
         sliderInput("rok", "Rok produkcji",
                        min(daneModelMarka()$Rok.produkcji),
                        max(daneModelMarka()$Rok.produkcji),
                        value=range(daneModelMarka()$Rok.produkcji))
   })
   
   output$kolor <- renderUI({
         selectInput("kolor", "Kolor", levels(auta2012$Kolor), multiple=TRUE)
   })   

   output$tabela <- renderTable({
      # czy uszkodzony?
      if(input$uszkodzony==TRUE)
         dane <- daneModelMarka() %>% filter(Pojazd.uszkodzony=="Tak") else
            dane <- daneModelMarka()  %>%  filter(Pojazd.uszkodzony!="Tak")
      # kolor?
      if(length(input$kolor)>0)
         dane <- dane %>% filter(Kolor%in%input$kolor)
      # reszta?
      dane %>%
         filter(Cena.w.PLN>=min(input$cena), Cena.w.PLN<=max(input$cena)) %>%
         filter(Rodzaj.paliwa%in%input$paliwo) %>%
         filter(Rok.produkcji>=min(input$rok), Rok.produkcji<=max(input$rok)) %>%
         filter(Przebieg.w.km<=input$przebieg | is.na(Przebieg.w.km)) %>%
         filter( grepl( paste0("^",as.character(input$drzwi)), Liczba.drzwi) ) %>%
         head(20)
   }) 

})
