library(PogromcyDanych)
library(shiny)

shinyServer(function(input,output){
    output$markaInput <- renderUI({
      marki <- unique(as.character(auta2012$Marka))
      names <- marki
      marki <- as.list(marki)
      names(marki) <- names
      selectInput("Marka", label=h4("Marka"), marki, selected=marki[[1]])
    })

    output$modelInput <- renderUI({
        modele <- filter(auta2012, Marka==input$Marka)$Model %>% 
                  as.character() %>% unique()
        selectInput("WybraneModele", h4("Model"), modele, selected=modele[[1]], multiple=TRUE)
      })
    
    output$cenaSlider <- renderUI({
        minimum <- min(auta2012$Cena.w.PLN)
        maximum <- max(auta2012$Cena.w.PLN)
        
        sliderInput("zakresCen", label=h4("Przedział cenowy w PLN:"),minimum, 
                    maximum, value=c(minimum, maximum))
        })
    
    output$paliwoRadio <- renderUI({
      rodzajPaliwa <- unique(as.character(auta2012$Rodzaj.paliwa))
      radioButtons("wybranePaliwo", h4("Rodzaj paliwa"), rodzajPaliwa, selected="benzyna")
      
      })
    
    output$tabela <- renderTable({

        tmp <- filter(auta2012, Marka==input$Marka, Model %in% input$WybraneModele,
                      Cena.w.PLN >=input$zakresCen[1], 
                      Cena.w.PLN <= input$zakresCen[2],
                      Rodzaj.paliwa==input$wybranePaliwo,
                      Rok.produkcji==input$rokInput,
                      Skrzynia.biegow==input$skrzyniaInput)
        #if(nrow(tmp)>100) return(tmp[1:100, ]) else return(tmp)
      
    })

   })
