library(shiny)
library(pakietWyboryPRW)
# library(stringi)
# library(ggplot2)
# library(tidyr)
# library(dplyr)


shinyUI(navbarPage("Projekt wybory",
                   tabPanel("Informacje ogolne",
                         h1("R i Big Data",align = "center"),
                         h2("Projekt Wybory", align = "center"),
                         h3("Mikolaj Wasniewski, Krzysztof Rudas, Pawel Pytlak", 
                            align = "center"),
                         
                         p("W okresie miedzy 10.03.2015, a 14.05.2015 zbieralismy ",
                           "dane z 4 portali informacyjnych (gazeta.pl, wiadomosci.onet.pl,
                           wiadomosci.wp.pl, tvn24.pl) oraz z Twittera 
                           (nasluch i oficjalne konta kandydatow). Zebralismy okolo 700 
                           unikalnych artykolow z portali informacyjnych i okolo 215 tys. tweetow z 
                           nasluchu i okolo 6,5 tys tweetow z oficjalnych kont kandydatow. ")
                   )
   ,
   tabPanel("Portale zmmiennosc w czasie",
      sidebarLayout(
         sidebarPanel(
            checkboxGroupInput("kandydaci_dzien",
                               "Wybierz kandydatow",
                               c("Braun", "Duda", "Jarubas", "Komorowski","Korwin-Mikke",
                                 "Kowalski", "Kukiz", "Ogorek", "Palikot", "Wilk"),
                              c("Komorowski","Duda","Kukiz")
            ),
            dateRangeInput("od_do_dzien","Podaj przedzial czasowy:",
                           start="03-13-2015", end= "05-15-2015",
                           min = "03-13-2015", max = "05-15-2015",
                           format="dd-mm-yyyy",
                           separator = " do "
            ),
            selectInput("zrodlo_dzien","Wybierz zrodlo danych:",
                        c("wiadomosci.onet.pl"="wiadomosci.onet.pl",
                          "wiadomosci.wp.pl"='wiadomosci.wp.pl',
                          "tvn24.pl"="tvn24.pl", 'gazeta.pl'="gazeta.pl",
                          "Wszystko"="brak"),
                        "brak"
            ),
            
            radioButtons("co_dzien", "Gdzie?", c("tytul"="tytul", "tresc"="tresc")),
            selectInput("liczbaserii_dzien", "Dlugosc serii",
                        3:20,
                        5)
         ),
         
         mainPanel(
            p("Wskazniki dla portali informacyjnych pokazujace zmiennosc w czasie"),
            br(),
            tabsetPanel(
               tabPanel("Liczba na dzien",
                        p("Liczba wystapien nazwisk wybranych kandydatow w tytule lub tresci 
                          na zadanym przedziale czasu i dla wybranego zrodla danych z podzialem na dni."),
                        br(),
                        plotOutput("wykres_ile_dziennie")),
               tabPanel("Wykres zliczen przez dni",
                        p("Zmiana liczba wystapien nazwisk wybranych kandydatow w tytule lub tresci 
                           na zadanym przediale czasu 
                          (liczba wystapien jest liczona od poczatku przedzialu czasu) i 
                          dla wybranego zrodla danych."),
                        br(),
                        plotOutput("wykresilezliczenprzezDni")),
               tabPanel("Procent dni wystapien", 
                        p("Wykres przedstawiajacy zmiane wartosci miary liczba dni",
                          " z wystapieniami/liczba dni wzgledem czasu."),
                        plotOutput("wykresprzezileDni")),
               tabPanel("Liczba serii",
                        p("Wykres przedstawiajacy zmiane wartosci liczby serii zadanej dlugosci
                           wystapien nazwisk wybranych kandydatow dla ustalonego przedzialu 
                           czasu."),
                        plotOutput("wykresLiczbySerii")),
               tabPanel("Maksymalna seria", 
                        p("Wykres przedstawiajacy zmiane wartosci maksymalnej serii wysapien 
                          nazwiska kandydata w tytule lub tresci na zadnym przedziale czasu 
                          i dla wybranego zrodla danych."),
                        plotOutput("wykresMaksymalnejSerii"))
               )
         )
      )
   ),
   # wskazniki nieliniowe
   tabPanel("Portale wskazniki ogolne",
      sidebarLayout(
         sidebarPanel(
            checkboxGroupInput("kandydaci_ilosc",
                               "Wybierz kandydatow:",
                               c("Braun", "Duda", "Jarubas", "Komorowski",
                                 "Korwin-Mikke","Kowalski", "Kukiz", 
                                 "Ogorek", "Palikot", "Wilk"),
                               c("Braun", "Duda", "Jarubas", "Komorowski",
                                 "Korwin-Mikke","Kowalski", "Kukiz", 
                                 "Ogorek", "Palikot", "Wilk")
            ),
            dateRangeInput("od_do_ilosc","Podaj przedzial czasowy:",
                           start="03-13-2015",  end= "05-15-2015",
                           min = "03-13-2015", max = "05-15-2015",
                           format="dd-mm-yyyy",
                           separator = " do "
            ),
            selectInput("zrodlo_ilosc","Wybierz zrodlo danych:",
                          c("wiadomosci.onet.pl"="wiadomosci.onet.pl",
                            "wiadomosci.wp.pl"='wiadomosci.wp.pl',
                            "tvn24.pl"="tvn24.pl", 'gazeta.pl'="gazeta.pl",
                           "Wszystko"="brak"),
                         "brak"
            ),
            radioButtons("co_ilosc", "Gdzie?", c("tytul"="tytul", "tresc"="tresc")),
            selectInput("liczbaserii_ilosc", "Dlugosc serii",
                        3:20,
                        5)
         ),
         
         mainPanel(
            p("Wskazniki dla portali informacyjnych"),
            tabsetPanel(
               tabPanel("Liczba serii",
                        br(),
                        p("Wykres slupkowy przedstawiajacy liczbe  serii zadanej dlugosci wystapien nazwiska 
                          wybranych kandydatow w tytule lub tresci.",
                          "Liczby serii sa zliczane na podanym przedziale czasu i zrodle danych"),
                        br(),
                        plotOutput("wykresLiczbySerii2")),
               
               tabPanel("Maksymalna seria",
                        br(),
                        p("Wykres slupkowy przedstawiajacy maksymalna serie wystapien nazwiska 
                          wybranych kandydatow na prezydenta w tytule lub tresci. 
                          Serie sa zliczane na zadanym przedziale czasu i zrodle danych"),
                        br(),
                        plotOutput("wykresMaksymalnejSerii2")),
               
               tabPanel("Liczba wystapien", 
                        br(),
                        p("Wykres rysujacy liczbe wystapien nazwiska wybranych kandydatow
                           w tytule lub w tresci w zadanym przedziale czasu.
                           Wykres jest rysowany dla wybranego zrodla danych."),
                        br(),
                        plotOutput("wykresilezliczenprzezDni2")),
               
               tabPanel("Boxplot czestotliwosci",
                        br(),
                        p("Wykres boxplot przedstawiajacy czestotliwosci pojawiania sie
                          artykow na temat wybranych kandydatow. Czestotliwosci sa 
                          liczone na zadanym przedziale czasu i zrodle danych."),
                        br(),
                        plotOutput("Boxplot_czestotliwosci")),
               
               tabPanel("Pora dnia", 
                        br(),
                        p("Wykres slupkowy przedstawiajacy liczbe wystapiem nazwisk 
                          wybranych kandydatow w tytule lub tresci z podzialem na pore dnia. 
                          Wskaznik jest liczony na zadanym przedziale czasu i zrodle danych. 
                          Przyjelismy nastepujacy podzial dnia: 8-12 rano, 12-18 w ciagu dnia, 
                          18-24 wieczorem, 24-6 w nocy."), 
                        br(),
                        plotOutput("wykres_pora_dnia"),
                        br()
                        ),
               
               tabPanel("Wykres rangi", 
                        br(),
                        p("Wykres slupkowy przedstawiajacy rangi dla wybranych kandydatow,
                          Rangi sa liczone dla podanego zakresu czasu. Ranga wystawiana na
                          zasadzie: 0 - zaden artykul sie nie pojawil,
                          1 - wartosc miary powyzej 168h(tydzien),
                          2 - wartosc miary ponizej 168h(tydzien), 3 - wartosc miary ponizej 96h(cztery doby),
                          4 - wartosc miary ponizej 48h(dwie doby),5 - wartosc miary ponizej 24h(doba)"),
                        br(),
                        plotOutput("wykresRangi")),
               
               tabPanel("Procent dni wystapien", 
                  br(),
                  p("Wykres slupkowy przedstawiajacy liczbe dni z wystapiem nazwisk 
                       wybranych kandydatow w tytule lub tresci podzielona przez liczbe dni. 
                       Wskaznik jest liczony na zadanym przedziale czasu i zrodle danych."), 
                  br(),
                  plotOutput("wykresprzezileDni2")
               
               ),
               
               tabPanel("Boxplot komentarzy", 
                   br(),
                   p("Boxplot przedstawiajacy liczbe komentarzy artykulow, w krorych 
                     wystapilo nazawisko wybrnych kandydatow w tytule lub w tresci.
                     Wskaznik jest liczony na zadanym przedziale czasu i zrodle danych."), 
                   br(),
                   plotOutput("wykres_komentarzy"))
            )
         )
      )
   ),
# oficjalny twitter
   tabPanel("Oficjalne tweety kandydatow",
         sidebarLayout(
            sidebarPanel(
               checkboxGroupInput("kandydaci_ofic",
                                  "Wybierz kandydatow:",
                                  c("Duda"="Andrzej Duda", 
                                    "Jarubas"="Adam Jarubas", 
                                    "Komorowski"="Bronislaw Komorowski",
                                    "Korwin-Mikke"="Janusz Korwin-Mikke", 
                                    "Kukiz"="Pawel Kukiz", 
                                    "Ogorek"="Magdalena Ogorek",
                                    "Palikot"="Janusz Palikot"),
                                  c("Bronislaw Komorowski","Andrzej Duda",
                                    "Pawel Kukiz")
               ),
               dateRangeInput("od_do_ofic","Podaj przedzial czasowy:",
                              start="2015-02-07", end= "05-15-2015",
                              min = "2015-02-07", max = "2015-05-15",
                              format="yyyy-mm-dd",
                              separator = " do "
               )
            ),
            
            mainPanel(
               p("Wskazniki dotyczace oficjalnych tweetow kandydatow"),
               tabsetPanel(
                  tabPanel("Liczba oficjalnych tweetow", 
                           p("Liczba tweetow z oficjalnych tweetow wybranych kandydatow 
                             z podzialem na dni."),
                           br(),
                           plotOutput("wykres_ilosc_ofic")),
                  tabPanel("Srednia polubien na dzien", 
                           p("Srednia liczba polubien tweetow wybranych knadydatow na 
                           prezydenta na dzien. 
                           Wykres jest rysowany na zadanym przedziale czasu i zrodle danych."),
                           plotOutput("wykres_ulubione_ofic")),
                  tabPanel("Srednia retwetow na dzien ", 
                           plotOutput("wykres_retweety_ofic")),
                  tabPanel("Sredni wydzwiek na dzien",
                           p("Sredni wydzwiek tweetow wybranych kandydatow na dzien.
                             Wykres rysowany na zadanym przedziale czasu." ),
                           plotOutput("wykres_wydzwiek_ofic"))
               )
            )
         )
   ),
tabPanel("Nasluch twittera",
         sidebarLayout(
            sidebarPanel(
               checkboxGroupInput("kandydaci_nasluch",
                                  "Wybierz kandydatow:",
                                  c("Duda"="Andrzej Duda", 
                                    "Jarubas"="Adam Jarubas", 
                                    "Komorowski"="Bronislaw Komorowski",
                                    "Korwin-Mikke"="Janusz Korwin-Mikke", 
                                    "Kukiz"="Pawel Kukiz", 
                                    "Ogorek"="Magdalena Ogorek",
                                    "Palikot"="Janusz Palikot"),
                                  c("Bronislaw Komorowski","Andrzej Duda",
                                    "Pawel Kukiz")
               ),
               dateRangeInput("od_do_nasluch","Podaj przedzial czasowy:",
                              start="2015-03-20", end= "05-15-2015",
                              min = "2015-03-20", max = "2015-05-15",
                              format="yyyy-mm-dd",
                              separator = " do "
               )
            ),
            mainPanel(
               p("Wskazniki z nasluchu twittera"),
               tabsetPanel(
                  tabPanel("Liczba tweetow na dzien", 
                           p("Liczba tweetow dotyczaca wybranych kandydatow 
                             z podzialem na dni. Wykres rysowany na zadanym przedziale czasu."),
                           br(),
                           plotOutput("wykres_ilosc_nasluch")),
                  tabPanel("Sredni wydzwiek na dzien",
                           p("Sredni wydzwiek tweetow dotyczacych wybranych kandydatow na dzien.
                             Wykres rysowany na zadanym przedziale czasu." ),
                           plotOutput("wykres_wydzwiek_nasluch"))
                  )
                  )
                  )
            )
      
))


