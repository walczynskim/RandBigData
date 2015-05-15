library(shiny)
library(dplyr)
library(PogromcyDanych)
carsDB <- auta2012
shinyUI(fluidPage(
  h1("Wybierz parametry samochodu:", align = "center"),
  fluidRow(
    column(3, selectInput("marka", label="Marka:", choices=levels(carsDB$Marka),
                       selected="")),
    column(3, textInput("model", "Model:", value="")),
    column(3, textInput("minRok", label="Min rok produkcji:",
                        value="1970")),
    column(3, textInput("maxRok", label="Max rok produkcji:",
      value="2012"))
  ),
  fluidRow(
    column(3, sliderInput("moc",  label="Moc silnika:", min=0, max=3000,
                          value=c(0,200))),
    column(3, textInput("przebieg", label="Max przebieg w km:",
                        value="200000")),
    column(3, sliderInput("cena", label="Cena:", min=0, max=250000,
                          value=c(10000,50000))),
    column(3, selectInput("sortuj", label="Sortuj po cenie", choices=c("rosnaco", "malejaco"),
                          selected="rosnaco"))
    
  ),
  fluidRow(
    column(3, selectInput("kolor", label="Kolor:", choices=levels(carsDB$Kolor),
      selected="")),
    column(3,selectInput("krajPochodzenia", label="Kraj pochodzenia:",
      choices=levels(carsDB$Kraj.pochodzenia), selected="")),
    column(3, checkboxGroupInput("liczbaDrzwi", label="Liczba drzwi:",
                                 choices=levels(carsDB$Liczba.drzwi)[-1])),
    column(3, checkboxGroupInput("paliwo", label="Rodzaj paliwa:", 
                                 choices=levels(carsDB$Rodzaj.paliwa)))
  ),
  mainPanel(
    h2("Lista samochodów:", align = "center"),
    tableOutput("output1")
  )
))
