library(shiny)
library(PogromcyDanych)
library(stringi)

auta <- auta2012

shinyUI(fluidPage(
  titlePanel("Wybierz idealny samochód dla siebie!"),
  sidebarLayout(
    sidebarPanel(      
      selectInput("marka",
                  "Wybierz marke", # opis kontrolki
                  levels(auta$Marka),
                  levels(auta$Marka)[2], multiple = TRUE),
      
      selectInput("model",
                  "Wybierz model", # opis kontrolki
                  levels(auta$Model),
                  levels(auta$Model)[2],multiple = TRUE),
      
      checkboxGroupInput("skrzynia_biegow",
                         "Jaka skrzynia biegów?",
                          levels(auta$Skrzynia.biegow)[!(stri_length((levels(auta$Skrzynia.biegow)))==0)], 
                          levels(auta$Skrzynia.biegow)[2]),
      
      dateInput("rok", "Wybierz rok produkcji", 
                format = "yyyy", 
                value = max(auta$Rok.produkcji),
                min = min(auta$Rok.produkcji), 
                max = max(auta$Rok.produkcji)),
      
      sliderInput("przebieg", "Maksymalny przebieg", 
                  value =  max(auta$Przebieg.w.km, na.rm = TRUE),
                  min = min(auta$Przebieg.w.km, na.rm = TRUE),
                  max = max(auta$Przebieg.w.km, na.rm = TRUE)),
      
      sliderInput("cena", "Przedzial cenowy", min = min(auta$Cena.w.PLN, na.rm = TRUE),
                  max = max(auta$Cena.w.PLN, na.rm = TRUE), 
                  value = c(0, max(auta$Cena.w.PLN, na.rm = TRUE))),
      
      selectInput("paliwo", "Rodzaj paliwa", levels(auta$Rodzaj.paliwa), 
                  levels(auta$Rodzaj.paliwa)[2], multiple = TRUE),
      
      selectInput("kolor", "Kolor", levels(auta$Kolor), 
                  levels(auta$Kolor)[2], multiple = TRUE),
      
      radioButtons("drzwi", "Liczba drzwi", 
                   levels(auta$Liczba.drzwi)[!(stri_length((levels(auta$Liczba.drzwi)))==0)], 
                   levels(auta$Liczba.drzwi)[2]),
      
      textInput("numer", "Podaj numer swojego telefonu", ""),
      
      helpText( "Teraz zostaną wybrane dla Ciebie samochody, 
                które odpowiadają twoim oczekiwaniom")
    ),
    
    mainPanel(
      p("Wybrane samochody:"),
      br(),
      tabsetPanel(
        tabPanel("Wybrane auta:", tableOutput("Tabela"))
      )
    )
  )
))

