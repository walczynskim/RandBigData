library(shiny)
library(PogromcyDanych)

shinyUI(fluidPage(
   titlePanel("Samochody"),
   sidebarLayout(
      sidebarPanel(

         h3("Jakiego auta szukasz?"),
         selectInput("marka", "Marka", levels(auta2012$Marka), multiple=TRUE),
         uiOutput("model"),
         uiOutput("cena"),
         uiOutput("rok"),
         checkboxGroupInput("paliwo", "Rodzaj paliwa", list(benzyna="benzyna",
                                                            benzynaLPG="benzyna+LPG",
                                                            etanol="etanol",
                                                            hybryda="hybryda",
                                                            elektryczny="naped elektryczny",
                                                            diesel="olej napedowy (diesel)"),
                            list(benzyna="benzyna",
                                 benzynaLPG="benzyna+LPG",
                                 etanol="etanol",
                                 hybryda="hybryda",
                                 elektryczny="naped elektryczny",
                                 diesel="olej napedowy (diesel)")),
         checkboxInput("uszkodzony", "Czy uszkodzony?"),
         uiOutput("kolor"),
         numericInput("przebieg","Maksymalny przebieg w km", 
                      value=max(auta2012$Przebieg.w.km[!is.na(auta2012$Przebieg.w.km)]),
                      min=min(auta2012$Przebieg.w.km[!is.na(auta2012$Przebieg.w.km)]), 
                      max=max(auta2012$Przebieg.w.km[!is.na(auta2012$Przebieg.w.km)])),
         textInput("wyposazenie", "Wazny element wyposazenia (po spacjach):"),
         numericInput("drzwi","Liczba drzwi-drzwi (przez które można wejść):", value=4)
      ),
      mainPanel(
                     tableOutput("tabela")
      )
   )
)
)
