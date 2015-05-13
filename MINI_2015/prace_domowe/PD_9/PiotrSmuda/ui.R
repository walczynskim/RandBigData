library(PogromcyDanych)
library(shiny)
library(dplyr)

auta<-PogromcyDanych::auta2012

shinyUI(fluidPage(
   titlePanel("Komis samochodowy - Twoje Przyszłe Auto"),

   sidebarLayout(
      sidebarPanel(

         h4("Wybierz parametry auta:"),

         selectInput(
            "marka",
            label="Marka:",
            choices=levels(auta$Marka),
            selected=""
         ),

         textInput(
            "model",
            "Podaj model:",
            value=""
         ),

         textInput(
            "max_rok_produkcji",
            label="Podaj maksymalny rok produkcji:",
            value="2012"
         ),

         textInput(
            "min_rok_produkcji",
            label="Podaj minimalny rok produkcji:",
            value="1900"
         ),

         sliderInput(
            "przebieg",
            label="Maksymalny przebieg w km:",
            min=0,
            max=2000000,
            value=300000
         ),

         sliderInput(
            "cena",
            label="Minimalna i maksymalna cena auta w PLN:",
            min=0,
            max=max(auta$Cena.w.PLN),
            value=c(0,100000)
         ),

         checkboxGroupInput(
            "paliwo",
            label="Rodzaj paliwa:",
            choices=levels(auta$Rodzaj.paliwa)
         ),

         sliderInput(
            "moc",
            label="Moc silnika w KM:",
            min=0,
            max=2500,
            value=c(0,100)
         ),

         sliderInput(
            "pojemnosc_skokowa",
            label="Pojemność skokowa w cm^3:",
            min=0,
            max=10000,
            value=c(0,2000)
         ),

         selectInput(
            "kraj_pochodzenia",
            label="Kraj pochodzenia:",
            choices=levels(auta$Kraj.pochodzenia),
            selected=""
         ),

         checkboxGroupInput(
            "liczba_drzwi",
            label="Liczba drzwi:",
            choices=levels(auta$Liczba.drzwi)[-1]
         ),

         checkboxGroupInput(
            "skrzynia_biegow",
            label="Skrzynia biegów:",
            choices=levels(auta$Skrzynia.biegow)[-1]
         ),

         selectInput(
            "kolor",
            label="Kolor:",
            choices=c(levels(auta$Kolor)),
            selected=""
         )

      ),

      mainPanel(
         p("Dostępne samochody o wskazanych parametrach:"),
         br(),
         tableOutput("tabela")
      )
   )
))
