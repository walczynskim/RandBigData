library(shiny)
library(PogromcyDanych)
library(dplyr)

shinyUI(fluidPage(

  sidebarLayout(
    sidebarPanel(
      
      textInput("text", label = "Jak masz na imie?", 
                value = "Enter text..."),
      
      dateInput("date", 
                label = "Jaka mamy dzis date?", 
                value = "2014-01-01"),
      
      selectInput("rodzaj_paliwa",
                  "Wybierz rodzaj paliwa:", 
                  levels(auta2012$Rodzaj.paliwa),
                  "benzyna",
                  multiple = TRUE),
      
      sliderInput("moc_silnika", 
                  label = "Wybierz moc silnika:",
                  min = range(na.omit(auta2012$kW))[1],
                  max = range(na.omit(auta2012$kW))[2],
                  value = c(100, 200)),
      
      sliderInput("maks_przebieg", 
                  label = "Wybierz maksymalny przebieg:",
                  min = range(na.omit(auta2012$Przebieg.w.km))[1],
                  max = 100000,
                  value = 100000, step = 1000),
      
      numericInput("min_rok_produkcji", 
                   label = "Wpisz minimalny rok produkcji:", 
                   value = 1990, 
                   min = min(auta2012$Rok.produkcji),
                   max = max(auta2012$Rok.produkcji)),
      
      numericInput("max_rok_produkcji", 
                   label = "Wpisz maksymalny rok produkcji:", 
                   value = 2008, 
                   min = min(auta2012$Rok.produkcji),
                   max = max(auta2012$Rok.produkcji)),
      
      checkboxGroupInput("opcje_dodatkowe", "Wybierz dodatkowe wyposazenie:",
                         choices = list("klimatyzacja" = "k", 
                                        "autoalarm" = "a", 
                                        "centralny zamek" = "c")
        ),
      
      helpText("Zauwaz, ze odznaczenie dodatkowego wyposazenia ", 
               "oznacza, ze ma go nie byc, a nie, ze jest to ",
               "obojetne."),
      
      selectInput("kraj",
                  "Wybierz kraj pochodzenie:", 
                  levels(auta2012$Kraj.pochodzenia),
                  "Polska"),
      
      checkboxInput("czy_rosnaco", "Czy sortowac po cenie rosnaco?", FALSE)
      
    ),
    
    mainPanel(
      tabPanel("Naglowek", verbatimTextOutput("podsumowanie")),
      tabPanel("Tabela", tableOutput("tabela"))
    )
  )
))
