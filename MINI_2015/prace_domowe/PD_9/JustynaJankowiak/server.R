library(shiny)
library(PogromcyDanych)
library(dplyr)

shinyServer(function(input, output, session) {
   
   dane <- auta2012
   dane$Rok.produkcji <- as.Date(strptime(dane$Rok.produkcji, "%Y"))
   
   observe({
      marka <- input$Marka
      ktore <- dane %>%
         filter(Marka == marka) %>%
         arrange(Model)
      updateSelectInput(session, "Model", 
                        choices = levels(factor(ktore$Model)), 
                        selected = levels(factor(ktore$Model))[1])
   })
   
   observe({
      marka <- input$Marka
      model <- input$Model
      ktore <- dane %>%
         filter(Marka == marka, Model == model)
         updateSliderInput(session, "Przebieg.w.km",
                           min = min(ktore$Przebieg.w.km, na.rm = TRUE), 
                           max = max(ktore$Przebieg.w.km, na.rm = TRUE),
                           value = max(ktore$Przebieg.w.km, na.rm = TRUE))   
   })


   dane.cena <- reactive({
      dane %>% 
         filter(Marka == input$Marka, Model == input$Model) %>%
         filter(Przebieg.w.km <= input$Przebieg.w.km) %>%
         filter(Liczba.drzwi == input$Liczba.drzwi) %>%
         filter(Rok.produkcji >= input$Daty[1] & Rok.produkcji <= input$Daty[2]) %>%
         filter(KM >= input$slider[1] & KM <= input$slider[2])
   })
   
  # odpowiedź na kontrolki  
  output$tabela <- renderTable({
     input$goButton
     isolate({
        dane.cena()
     }) 
  })

  output$boxplot <- renderPlot({ 
        if(input$radio == 1) boxplot(dane.cena()$Cena.w.PLN, col = "green")
        else boxplot(dane.cena()$Cena.w.PLN, col = "red")
  })
  
  output$histogram <- renderPlot({
     hist(dane.cena()$Cena.w.PLN)
  })
  
  output$podsumowanie <- renderPrint({
     summary(dane.cena()$Cena.w.PLN)
  })
  
  output$twojeImie <- renderText({
     input$Imie
  })
  
})

