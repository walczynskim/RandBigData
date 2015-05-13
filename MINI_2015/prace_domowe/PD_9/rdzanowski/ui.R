library(shiny)
library(PogromcyDanych)

auta <- PogromcyDanych::auta2012
auta <- cbind(auta, Cena.w.PLN.brutto = ifelse(auta$Brutto.netto == "brutto", auta$Cena.w.PLN, floor(1.23*auta$Cena.w.PLN)))

shinyUI(fluidPage(
  
  headerPanel("Znajdź auto marzeń!"),
  titlePanel("Wybierz parametry auta"),
  
  sidebarPanel(
    sliderInput("cena",
                "Minimalna i maksymalna cena brutto auta w PLN*1000",
                min = min(auta$Cena.w.PLN.brutto)/1000,
                max = max(auta$Cena.w.PLN.brutto)/1000,
                value = c(quantile(auta$Cena.w.PLN.brutto, probs = 0.25), 4000000)/1000),
    selectInput("waluta", 
                label = "Waluta zakupu", 
                choices = c("dowolna", levels(auta$Waluta)), 
                selected = "PLN"),
    textInput("marka", 
              "Podaj nazwę marki auta (na przykład Kia, dowolna)", 
              value = "dowolna"), 
    textInput("model", 
              "Podaj nazwę modelu auta (na przykład Carens, dowolny)", 
              value = "dowolny"), 
    sliderInput("przebieg",
                "Przebieg auta w 1000 km",
                min = min(auta$Przebieg.w.km, na.rm = TRUE)/1000,
                max = max(auta$Przebieg.w.km, na.rm = TRUE)/1000,
                value = c(quantile(auta$Przebieg.w.km, probs = 0.25, na.rm = TRUE), quantile(auta$Przebieg.w.km, probs = 0.85, na.rm = TRUE))/1000), 
    sliderInput("rok_produkcji",
                "Rok produkcji:",
                min = min(auta$Rok.produkcji, na.rm = TRUE),
                max = max(auta$Rok.produkcji, na.rm = TRUE),
                value = c(1995, 2010)),
    textInput("skrzynia", 
              "Podaj rodzaj skrzyni biegów (manualna, automatyczna, dowolna)", 
              value = "dowolna"), 
    sliderInput("pojemnosc",
                "Pojemność skokowa:",
                min = min(auta$Pojemnosc.skokowa, na.rm = TRUE),
                max = max(auta$Pojemnosc.skokowa, na.rm = TRUE),
                value = c(quantile(auta$Pojemnosc.skokowa, probs = 0.25, na.rm = TRUE), quantile(auta$Pojemnosc.skokowa, probs = 0.95, na.rm = TRUE))), 
    selectInput("drzwi", 
                label = "Liczba drzwi:", 
                choices = c("dowolna", levels(auta$Liczba.drzwi)), 
                selected = "dowolna"), 
    sliderInput("kon",
                "Moc silnika w KM",
                min = min(auta$KM, na.rm = TRUE),
                max = max(auta$KM, na.rm = TRUE),
                value = c(quantile(auta$KM, probs = 0.25, na.rm = TRUE), 5000))    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    p("Dane wybranych aut:"),
    br(),
    tabsetPanel(
      tabPanel("Przykładowe auta", tableOutput("tabela")),
      tabPanel("Histogram cen aut", plotOutput("histogram"))
    )
  )
))
