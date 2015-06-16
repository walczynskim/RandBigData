library(shiny)
library(projektFilmy)
library(stringi)

dane <- projektFilmy::filmy[1:500,]

shinyUI(fluidPage(
  titlePanel("Po co tracić czas na oglądanie nieciekawych filmów? Dzięki naszej aplikacji wybierzesz te, które interesują właśnie Ciebie!",),
  sidebarLayout(
    sidebarPanel(
      selectInput("film",
                  h3("WYBIERZ FILM"),
                  dane$tytul_filmweb,
                  "Poranek kojota"),
#       helpText("Wybierz film, do którego chcesz szukać filmów podobnych!"),
      numericInput("ile", 
                   label = h3("ILU FILMÓW SZUKASZ?"), 
                   value = 8,
                   min = 1,
                   max = 20),
      
      h3("NADAJ WAGI:"),
      
      numericInput("czas", 
                   label = "Czas (m.in. podobny rok produkcji):", 
                   value = 1, 
                   min = 0,
                   max = 100),
      numericInput("ocena", 
                   label = "Ocena (m.in. zbliżona ocena filmów):", 
                   value = 1, 
                   min = 0,
                   max = 100),
      numericInput("widocznosc", 
                   label = "Widoczność (m.in. liczba komentarzy, liczba wyświetleń):", 
                   value = 1, 
                   min = 0,
                   max = 100),
      numericInput("geo", 
                   label = "Miejsce (m.in. kraj produkcji):", 
                   value = 4, 
                   min = 0,
                   max = 100),
      numericInput("naj", 
                   label = "Tagi (m.in. tagi do filmów):", 
                   value = 10, 
                   min = 0,
                   max = 100)
#       helpText("Nadaj wagi konkretnym kategoriom (skala od 0 do 100)", 
#                "tak, żeby najlepiej wyrazić swoje preferencje, ",
#                "co do szukanych filmów!")
      
      ),
    
    
#     numericInput("czas", 
#                  label = "Czas (m.in. podobny rok produkcji):", 
#                  value = 1990, 
#                  min = 0,
#                  max = 100),    

    
    mainPanel(

      tabsetPanel(
        tabPanel("INSTRUKCJA", verbatimTextOutput("instrukcja")),
        tabPanel("TABELA", tableOutput("tabela")),
        tabPanel("WYKRES 2D", plotOutput("wykres2d")),
        tabPanel("WYKRES 3D", plotOutput("wykres3d")),
        tabPanel("WYKRES 3D (obracalny)", plotOutput("wykres3d_nowy"))
      )
    )
  )
))
