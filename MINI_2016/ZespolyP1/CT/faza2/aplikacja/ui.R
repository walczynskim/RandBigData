library(shiny)

load("pstwa.rda")
eksponaty <- rownames(pstwa)

shinyUI(fluidPage(
  tags$head(tags$style(HTML("
                            .well {
                            width: 300px;
                            }
                            "))),
  titlePanel("Prawdopodbieństwa przejść"),
  sidebarLayout(
    sidebarPanel(
      selectInput("wybranyEksponat", 
                  label = "Wybierz eksponat",
                  choices = eksponaty,
                  selected = "cnk02a"),
      sliderInput("suwak", "Wybierz liczbę eksponatów", 
                  min = 1, max = 45, value = 5, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela", 
                 p("Prawdopodobieństwa przejscia dla wybranego eksponatu"), 
                 dataTableOutput("Tabela")),
        tabPanel("Wykres",
                 p(" "),
                 plotOutput("wykres", click = "klik")),
        tabPanel("Eksponaty", 
                 plotOutput("mapa", click = "klik_mapa", 
                            width = 650, height = 500)),
        tabPanel("Scieżki", 
                 plotOutput("mapa2", 
                            width = 650, height = 500))
      )
    )
  )
  ))