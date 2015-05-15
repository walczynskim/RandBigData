
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(PogromcyDanych)
library(dplyr)
library(stringi)
library(ggplot2)

shinyUI(fluidPage(

  # Application title
  titlePanel("Analiza danych o samochodach"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("marka",
                  "Marka samochodu:",
                  levels(auta2012$Marka), selected="Porsche", multiple=TRUE),
      radioButtons("skrzynia","Skrzynia biegow:",c("automatyczna","manualna")),
      selectInput("model",
                  "Model samochodu:",
                    levels(auta2012$Model),multiple=TRUE),
      sliderInput("cena", "Cena:",
                  min = 1, max = 20, value = c(5, 15)),
      sliderInput("przebieg", "Przebieg (w km):",min=0,
                  max = 20, value = c(5)),
      checkboxGroupInput("rodzaj_paliwa","Rodzaj paliwa:", choices=levels(auta2012$Rodzaj.paliwa)),
      numericInput("km", "Minimalna ilosc koni mechanicznych:",value=0),
      checkboxInput("czy_pln", "Waluta w PLN:", value = TRUE),
      selectInput("kraj", "Kraj pochodzenia:", choices=c(levels(auta2012$Kraj.pochodzenia),"Obojetnie"),
                                                         selected="Obojetnie"),
      textInput("dane","Podaj swoje imie i nazwisko"),
      textInput("wiek","Podaj swoj wiek"),
      dateInput("data2","data",format="yyyy",value="1990")
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
      tabPanel("POWITANIE",verbatimTextOutput("info")),
      tabPanel("TABELA",tableOutput("dane_analizowane")),
      tabPanel("WYKRES",plotOutput("wykres",width=900))
    ))
  )
))

