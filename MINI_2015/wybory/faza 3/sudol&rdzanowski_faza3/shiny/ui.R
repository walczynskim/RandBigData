library(shiny)
library(lazyeval)
library(wsk)
library(ggplot2)
library(dplyr)

dane <- wsk::wybory2015

shinyUI(fluidPage(
  
  headerPanel("Obejrzyj wskazniki widocznosci kandydatow w sieci!"),
  titlePanel("Wybierz interesujace Cie wskazniki, osoby i portale:"),

  sidebarLayout(
    sidebarPanel(
      selectInput("wskaznik", 
                  label = "Wybierz wskaznik:", 
                  choices = c("p-stwo widocznosci kandydata", 
                              "p-stwo pozytywnej widocznosci kandydata", 
                              "p-stwo negatywnej widocznosci kandydata", 
                              "p-stwo neutralnej widocznosci kandydata", 
                              "szansa pozytywnej widocznosci kandydata", 
                              "szansa negatywnej widocznosci kandydata", 
                              "szansa neutralnej widocznosci kandydata", 
                              "stosunek tekstow pozytywnych do negatywnych"), 
                  selected = "p-stwo widocznosci kandydata"), 
      selectInput("sentyment", 
                  label = "Wybierz interesujacy Cie sentyment:",
                  choices = c("sentyment_1", "sentyment_2"), 
                  selected = "sentyment_1"),
      checkboxGroupInput("kandydat", 
                         label = "Wybierz kandydatow na prezydenta:", 
                         choices = c("B. Komorowski",
                                     "A. Duda", 
                                     "P. Kukiz", 
                                     "J. Korwin-Mikke", 
                                     "M. Ogorek", 
                                     "A. Jarubas", 
                                     "J. Palikot"), 
                         selected = c("B. Komorowski",
                                      "A. Duda", 
                                      "P. Kukiz", 
                                      "J. Korwin-Mikke", 
                                      "M. Ogorek", 
                                      "A. Jarubas", 
                                      "J. Palikot"), 
                         inline = FALSE),
      checkboxGroupInput("portal", 
                         label = "Wybierz interesujace Cie portale internetowe:",
                         choices = c("gazeta.pl",
                                     "newsweek.pl", 
                                     "wp.pl",
                                     "onet.pl", 
                                     "onet.pl/wybory", 
                                     "tvn24.pl", 
                                     "tvn24.pl/wybory", 
                                     "tvpinfo.pl", 
                                     "twitter_kandydat", 
                                     "twitter_nasluch"), 
                         selected = c("gazeta.pl",
                                      "newsweek.pl", 
                                      "wp.pl",
                                      "onet.pl", 
                                      "onet.pl/wybory", 
                                      "tvn24.pl", 
                                      "tvn24.pl/wybory", 
                                      "tvpinfo.pl", 
                                      "twitter_kandydat", 
                                      "twitter_nasluch"), 
                         inline = TRUE), 
      selectInput("smooth", 
                  label = "Dorysuj krzywe regresji:", 
                  choices = c("lm", "loess", "nie rysuj"), 
                  selected = "nie rysuj")
    ),

    mainPanel(
      p("Wskazniki widocznosci kandydatow w sieci."),
      br(),
      tabsetPanel(
        tabPanel("Tabela wskaznikow", tableOutput("tabela")),
        tabPanel("Wykres wskaznikow", plotOutput("wykres1")),
        tabPanel("Wykres wskaznika twitt_poz_neg", plotOutput("wykresik"))
      )
    )
  )
))
