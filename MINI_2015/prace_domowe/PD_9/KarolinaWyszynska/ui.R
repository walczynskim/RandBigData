library(shiny)

shinyUI(fluidPage(
   titlePanel("KOMIS SAMOCHODOWY"),
   sidebarLayout(
      sidebarPanel(
         h3("Jakiego auta szukasz?"),
         uiOutput("markaInput"),
         uiOutput("modelInput"),
         uiOutput("cenaSlider"),
         uiOutput("paliwoRadio"),
         checkboxGroupInput("skrzyniaInput", label=h4("Skrzynia biegów"), 
                            choices=list(Manualna="manualna", 
                                         Automatyczna="automatyczna",
                                         Nieokreślono=""), selected="manualna"),
         numericInput("rokInput", label=h4("Rok produkcji"), value=2000)
         
         ),
      mainPanel(
        tabsetPanel(
          tabPanel("Główna", 
                h3("Witaj!"),
                br(),
                "Znajdujesz się w serwisie komisu samochodowego. 
                Mamy auta zarówno nowe jak i używane. W opcjach z lewej strony 
                zaznacz czego szukasz, a na pewno znajdziesz u nas coś dla siebie.",
                br(),
                "Udanych zakupów!",
                br(),
                tableOutput("tabela")
                ),
          tabPanel("Kontakt", br(),
                   "Jeśli nie zadowoliła Cię nasza oferta prosimy zostaw nam swoje dane kontaktowe.
                   Nasz konsultant zatelefonuje do Ciebie i znajdzie coś specjalnego!",
                   br(),
                   textInput("imie", h4("Imię"), ""),
                   textInput("nazwisko", h4("Nazwisko"),""),
                   textInput("telefon", "Nr telefonu kontaktowego",""),
                   br(),
                   "Dziękujemy za wizytę w naszym serwisie! :) "
     
          )
        )
      )
   )
  )
)
