library(shiny)
library(stringi)
library(dplyr)

#sciezka<-getwd()
sciezka <- "../../../materialy/"
load(file.path(sciezka,"wynikiDF.rda"))

wynikiDF<-as.data.frame.matrix(wynikiDF)

#przy pracy na Windows'ie trzeba kodowanie zmienic
#wynikiDF$oferta<-suppressWarnings(stri_encode(wynikiDF$oferta,"UTF-8","CP1250"))
#wynikiDF$lokacja<-suppressWarnings(stri_encode(wynikiDF$lokacja,"UTF-8","CP1250"))
#wynikiDF$firma<-suppressWarnings(stri_encode(wynikiDF$firma,"UTF-8","CP1250"))
#wynikiDF$stanowisko<-suppressWarnings(stri_encode(wynikiDF$stanowisko,"UTF-8","CP1250"))

sport <- stri_detect_fixed(wynikiDF$oferta, " sport")
multisport <- stri_detect_fixed(wynikiDF$oferta, "multisport")

wynikiDF$multisport <- multisport

lista_firma_sport <- sort(wynikiDF[sport,]$firma %>% unique())
lista_firma_multisport <- sort(wynikiDF[multisport,]$firma %>% unique())

shinyUI(fluidPage(
 
  titlePanel("Analiza ofert z pracuj.pl pod względem pakietu sportowego"),
  sidebarLayout(
    sidebarPanel(
       
       conditionalPanel('input.dataset === "Sport"', 
                        radioButtons("jaki_wybor", "Wyszukiwanie ogłoszenia według:",
                                     c("daty", "firmy","firmy i daty")),
                        conditionalPanel('input.jaki_wybor==="daty"',
                                         selectInput("data", 
                                                     label = "Wybrana data ogłoszenia",
                                                     choices = as.character(sort(as.Date(unique(wynikiDF$data)))),
                                                     selected = as.character(as.Date(wynikiDF$data[1])) ) ),
                        conditionalPanel('input.jaki_wybor==="firmy"', 
                                         selectInput("firma_sport", 
                                                     label = "Ogloszenia dla firmy:",
                                                     choices = as.character(lista_firma_sport),
                                                     selected = as.character(lista_firma_sport[1]) )),
                        conditionalPanel('input.jaki_wybor==="firmy i daty"',
                                         selectInput("firma_sport", 
                                                     label = "Ogloszenia dla firmy:",
                                                     choices = as.character(lista_firma_sport),
                                                     selected = as.character(lista_firma_sport[1]) ),
                                         selectInput("data", 
                                                     label = "Wybrana data ogłoszenia",
                                                     choices = as.character(sort(as.Date(unique(wynikiDF$data)))),
                                                     selected = as.character(as.Date(wynikiDF$data[1])) )
                        )
       ),
       
       conditionalPanel('input.dataset === "Multisport"', 
                        radioButtons("jaki_wybor", "Wyszukiwanie ogłoszenia według:",
                                     c("daty", "firmy","firmy i daty")),
                        conditionalPanel('input.jaki_wybor==="daty"',
                                         selectInput("data", 
                                                     label = "Wybrana data ogłoszenia",
                                                     choices = as.character(sort(as.Date(unique(wynikiDF$data)))),
                                                     selected = as.character(as.Date(wynikiDF$data[1])) ) ),
                        conditionalPanel('input.jaki_wybor==="firmy"', 
                                         selectInput("firma_multisport",
                                                     label = "Ogloszenia dla firmy:",
                                                     choices = as.character(lista_firma_multisport),
                                                     selected = as.character(lista_firma_multisport[1]) )),
                        conditionalPanel('input.jaki_wybor==="firmy i daty"',
                                         selectInput("firma_multisport",
                                                     label = "Ogloszenia dla firmy:",
                                                     choices = as.character(lista_firma_multisport),
                                                     selected = as.character(lista_firma_multisport[1]) ),
                                         selectInput("data", 
                                                     label = "Wybrana data ogłoszenia",
                                                     choices = as.character(sort(as.Date(unique(wynikiDF$data)))),
                                                     selected = as.character(as.Date(wynikiDF$data[1])) )
                        )
       )

    ),
    mainPanel(
      tabsetPanel(
         id = 'dataset',
         tabPanel("Sport", 
                  dataTableOutput("sport")
         ),
         tabPanel("Sport - statystyki",
                  plotOutput("woj_s"),
                  plotOutput("stan_s")
         ),
         tabPanel("Multisport",
                  dataTableOutput("multisport")
         ), 
         tabPanel("Multisport - statystyki",
                  plotOutput("woj"),
                  plotOutput("stan")
         )
         
      )
    )
  )
  ))