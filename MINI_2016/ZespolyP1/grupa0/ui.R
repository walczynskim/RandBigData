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
         conditionalPanel('input.dataset === "Ścieżka - liczba ekpsponatów"',
                          selectInput("e_daty_rok", "Wybór roku:", 
                                      choices=c("2012","2013","wszystkie"),selected = "2012" ),
                          selectInput("e_daty_miesiac", "Wybór miesiaca:", selected = "02",
                                      choices=c("01","02","03","04","05","06","07","08","09","10","11","12","wszystkie")),
                          selectInput("e_daty_dzien", "Wybór dnia:", selected=1,
                                      choices=c(1:31,"wszystkie"))
         ),
         conditionalPanel(' input.dataset === "Ścieżka - czas interakcji"',
                          selectInput("c_daty_rok", "Wybór roku:", 
                                      choices=c("2012","2013") ),
                          selectInput("c_daty_miesiac", "Wybór miesiaca:", selected = "01",
                                      choices=c("01","02","03","04","05","06","07","08","09","10","11","12")),
                          selectInput("c_daty_dzien", "Wybór dnia:", selected="1",
                                      choices=c(1:31,"maks"))
         ),
         conditionalPanel('input.dataset === "Rozkład czasów"',
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
                          selectInput("t_daty_rok", "Wybór roku:", 
                                      choices=c("2012","2013") , selected = "2012"),
                          selectInput("t_daty_miesiac", "Wybór miesiaca:", selected = "02",
                                      choices=c("01","02","03","04","05","06","07","08","09","10","11","12")),
                          selectInput("t_daty_dzien", "Wybór dnia:", selected="01",
                                      choices=c("01","02","03","04","05","06","07","08","09",as.character(10:31))),
                          radioButtons("t_zmienna", "Wybór popularnosci sciezki:", selected = "glowka",
                                       c("najbardziej popularne"="glowka", 
                                         "najmniej popularne"="stopka"))
         )

      ),
      mainPanel(
         tabsetPanel(
            id = 'dataset',
            tabPanel( "Instrukcja obsługi",
               p("Aplikacja służy do analizy ścieżek odwiedzających Centrum Nauki Kopernik w latach 2012-2013."),
               
               p("Użytkownik ma do wyboru 5 typów wykresów. Wykresy zmieniamy klikając na odpowiedni tytuł wykresu. "),
               
               p("W wykresie 'Ścieżka - liczba eksponatów' możemy dokonać wyboru daty, dla której chcemy uzyskać wykres. 
                 Należy wskazać rok, miesiąc i dzień odpowiednio. 
                 Wybierając w dniach 'wszytskie' uzyskujemy trzy najdłuższe ścieżki w danym miesiącu. 
                 W niektóre dni CNK było nieczynne. Dokonując wyboru takiego dnia nie uzyskamy żadnego wykresu. 
                 Na wykesie na osi rzędnych przedstawione są numery eksponatów w kolejności występowania w CNK, 
                 zaś oś odciętych numery kolejnych ekspsonatów do których podszedł użytkownik. 
                 Pod wykresem zajduje się tabela z wypisanymi kolejno eksponatami do których logował się zwiedzający."),

               p("Wykres 'Ścieżka - czas interakcji' przedstawia najdłuższą ścieżkę pod względem czasu spędzonego przy eksponatach. 
                  Możemy tutaj dokonać wyboru roku, miesiaca oraz dnia dla którego chcemy otrzymać wykres. 
                  Wskazując w dniu 'maks' otrzymamy wykres najdłuższej ścieżki w wybranym miesiącu. 
                  Na osi rzędnych znajduje sie czas spędzony przy eksponacie podany w sekundach, 
                  zaś na osi odciętych numery kolejnych ekspsonatów do których podszedł użytkownik. 
                  Nad każdym z punktów na wykresie znajdziemy również numer opisywanej stacji."),

               p("Wykres 'Typowa ścieżka' prezentuje typową ścieżkę składającą się z 8 eksponatów. 
                  Tytaj również możemy dokonać wyboru daty jak w poprzednich wykresach. 
                  Dodatkowo możemy wskazać  czy chcemy najbardziej popularne czy najmniej popularne ścieżki. 
                  Wówczas zmieni nam się tabela z nazwami eksponatów i liczbą odwiedzin znajdująca się pod wykresem. 
                  Osie opisane są analigicznie jak w pierszym wykresie. "),

               p("Wykres 'Rozkład czasów' przedstawia rozkład czasów dla najdłuższej ścieżki pod względem liczby odwiedzających. 
                  Możemy tutaj dokonać wyboru przedziału czasu dla którego chcemy otrzymać wykres zmieniając 'wybór okresu'. 
                  Na osi rzędnych znajdują się wybrane przez nas okresy czasów, a na osi odciętych liczby odwiedzanych eksponatów. 
                  Zastosowaliśmy tutaj wykres pudełkowy który obrazuje zmiany w średnich (niebieska pionowa kreska) 
                  i medianach (biała kropka) w zależności od odstępów czasu. Rwnież cziekawe są obserwacje odstające, 
                  które łatwo mozemy zaobserwować na tym wykresie. "),

               p("Ostatni wykres 'Wordcloud' prezentuje napisy z nazwami najpopularniejszych eksponatów w miesiącu. 
                  Wyboru roku i miesiąca dokonujemy na listach po lewej stronie. 
                  Największy napis na wykresie odpowiada najczęściej odwiedzanemu eksponatowi. Im mniejszy napis tym popularność mniejsza. ")
            ),
            tabPanel("Ścieżka - liczba ekpsponatów",
                     plotOutput("naj_eksponat"),
                     dataTableOutput("naj_eksponat2")
            ),
            tabPanel("Ścieżka - czas interakcji",
                     plotOutput("naj_czas")
            ),
            tabPanel("Typowa ścieżka",
                     plotOutput("typ"),
                     dataTableOutput("typ2")
            ),
            tabPanel("Rozkład czasów",
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