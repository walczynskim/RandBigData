library(shiny)
library(PogromcyDanych)

dane <- auta2012
dane$Rok.produkcji <- as.Date(strptime(dane$Rok.produkcji, "%Y"))

shinyUI(fluidPage(
  titlePanel("Znajdź swój samochód!"),
  sidebarLayout(
    sidebarPanel(
      helpText("Wybierz parametry, które powinien mieć twój wymarzony samochód i sprawdź ile kosztuje."),
      selectInput("Marka",
                  "Wybierz markę",
                  levels(factor(dane$Marka)),
                  levels(factor(dane$Marka))[1]),
      
      selectInput("Model",
                  "Wybierz model",
                  levels(factor(dane$Model)),
                  levels(factor(dane$Model))[1]),
      
      sliderInput("Przebieg.w.km",
                  "Podaj maksymalny przebieg",
                  min = min(dane$Przebieg.w.km, na.rm = TRUE), 
                  max = max(dane$Przebieg.w.km, na.rm = TRUE), 
                  value = max(dane$Przebieg.w.km, na.rm = TRUE)),
    
     checkboxGroupInput("Liczba.drzwi",
                        "Podaj liczbę drzwi",
                        levels(dane$Liczba.drzwi), 
                        levels(dane$Liczba.drzwi)),
     
     sliderInput("slider", label = "Ilość koni mechanicznych", 
                 min = min(dane$KM, na.rm = TRUE), 
                 max = 1000, 
                 value = c(min(dane$KM, na.rm = TRUE), 1000)),
     
     dateRangeInput("Daty", label = "Podaj zakres roczników:",
                    format="yyyy", separator = "-",
                    start = min(strptime(dane$Rok.produkcji, "%Y")),
                    end = max(strptime(dane$Rok.produkcji, "%Y"))),
     
     radioButtons("radio", label = "Jaki masz dzisiaj humor",
                  choices = list("dobry" = 1, "zly" = 2), 
                  selected = 1),
     
     textInput("Imie",
               "Podaj swoje imię",
               ""),
     
     actionButton("goButton", "Pokaż szczegóły"),
     
     helpText("Po wciśnięciu tego przycisku w zakładce Tabela pokażą się szczegóły wybranych samochodów.")
     
    ),
    
    mainPanel(
      h1(textOutput("twojeImie")),
      h3("Ceny dla wybranych przez ciebie samochodów"),
      tabsetPanel(
        tabPanel("Boxplot", plotOutput("boxplot", width = 500)),
        tabPanel("Histogram", plotOutput("histogram", width = 500)),
        tabPanel("Podsumowanie", verbatimTextOutput("podsumowanie")),
        tabPanel("Tabela", tableOutput("tabela"))
      )
    )
  )
))
