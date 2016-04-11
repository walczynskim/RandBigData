library(shiny)
library(stringi)
library(grupa0)
library(DT)


#przy pracy na Windows'ie trzeba kodowanie zmienic
#sciezka_eksponat$dzien_tyg <-suppressWarnings(stri_encode(sciezka_eksponat$dzien_tyg,from="UTF-8",to="Windows 1250"))

lista<-levels(typowa_sciezka$data)

shinyUI(fluidPage(
   
   titlePanel("Analiza scieżek odwiedzających Centrum Nauki Kopernik"),
   sidebarLayout(
      sidebarPanel(
         conditionalPanel('input.dataset === "Najdłużasza ścieżka pod względem liczby ekpsponatów"',
                          selectInput("e_daty_rok", "Wybór roku:", 
                                      choices=c("2012","2013","wszystkie"),selected = "2012" ),
                          selectInput("e_daty_miesiac", "Wybór miesiaca:", selected = "01",
                                      choices=c("01","02","03","04","05","06","07","08","09","10","11","12","wszystkie")),
                          selectInput("e_daty_dzien", "Wybór dnia:", selected=1,
                                      choices=c(1:31,"wszystkie"))
         ),
         conditionalPanel(' input.dataset === "Najdłużasza ścieżka pod względem czasu"',
                          selectInput("c_daty_rok", "Wybór roku:", 
                                      choices=c("2012","2013") ),
                          selectInput("c_daty_miesiac", "Wybór miesiaca:", selected = "01",
                                      choices=c("01","02","03","04","05","06","07","08","09","10","11","12")),
                          selectInput("c_daty_dzien", "Wybór dnia:", selected="1",
                                      choices=c(1:31,"maks"))
         ),
         conditionalPanel('input.dataset === "Rozkład czasów dla najdłuższej ścieżki"',
                          radioButtons("jaki_wybor", "Wybór okresu:", selected = "rok",
                                       c("rok", "kwartal", "miesiac", "dzien_tygodnia", "dzien_miesiaca","zagregowane"))
         ),
         conditionalPanel(' input.dataset === "Wordcloud"',
                          selectInput("w_daty_rok", "Wybór roku:", 
                                      choices=c("2012","2013"),selected = "2012" ),
                          selectInput("w_daty_miesiac", "Wybór miesiaca:", selected = "01",
                                      choices=c("01","02","03","04","05","06","07","08","09","10","11","12"))
         ),
         conditionalPanel(' input.dataset === "Typowa ścieżka"',
                          selectInput("t_daty_dzien", "Wybór dnia:", selected=lista[1],
                                      choices=lista),
                          radioButtons("t_zmienna", "Wybór popularnosci sciezki:", selected = "glowka",
                                       c("najbardziej popularne"="glowka", 
                                         "najmniej popularne"="stopka"))
         )

      ),
      mainPanel(
         tabsetPanel(
            id = 'dataset',
            tabPanel("Najdłużasza ścieżka pod względem liczby ekpsponatów",
                     plotOutput("naj_eksponat")
            ),
            tabPanel("Najdłużasza ścieżka pod względem czasu",
                     plotOutput("naj_czas")
            ),
            tabPanel("Typowa ścieżka",
                     plotOutput("typ"),
                     dataTableOutput("typ2")
            ),
            tabPanel("Rozkład czasów dla najdłuższej ścieżki",
                     plotOutput("rozkl")
            ),
            tabPanel("Wordcloud",
                     plotOutput("wcloud")
            )
            
            )
            
         )
      )
   )
)