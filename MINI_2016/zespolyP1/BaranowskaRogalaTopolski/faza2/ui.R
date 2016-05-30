library(shiny)
library(dplyr)
library(stringi)
library(png)
library(sciezkiCNK)
library(igraph)
appTypes <- as.list(1:2)
names(appTypes)<- c("Dni w roku",  "Miesiące, dni tygodnia oraz godziny")

shinyUI(fluidPage(
  titlePanel("Jak poruszamy się po CNK?"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "AppType",
                  label = "Wybierz jak chcesz agregować ścieżki",
                  choices = appTypes,
                  selected = 1),
      htmlOutput("SelektorDni"),
      htmlOutput("SelektorMiesiaca"),
      htmlOutput("SelektorMiesiacaCustom"),
      htmlOutput("SelektorDniaTygodnia"),
      htmlOutput("SelektorDniaTygodniaCustom"),
      sliderInput(inputId = "godzina", 
                  label = "Wybierz zakres godzin",
                  min = 9,
                  max = 20,
                  value = c(9,20),
                  step = 1,
                  round = TRUE),
      checkboxInput(inputId = "czySciezka",
                    label = "Typowa ścieżka",
                    value = TRUE),
      htmlOutput("SelektorDlugosciSciezki"),
      htmlOutput("text")
    ),
    mainPanel(
      plotOutput("sciezki", width = "100%", hover = "plot_hover")
    )
  )
))
