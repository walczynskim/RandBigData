library(shiny)
load("selectedTitles.rda", envir = .GlobalEnv)  # zawiera obiekt NazwyTytulow.

shinyUI(fluidPage(theme="bootstrap.css",
   h2("Witaj w MEGUSTA!", align="center"),
   h3("Mistrzowskim Estymatorze Gustu Audiowizualnego", align="center"),
   h4("Powiedz nam kim jesteś i jakie filmy lubisz, a my Ci powiemy jakie filmy polubisz!", align="center"),
   sidebarLayout(
      sidebarPanel(
         h4("Jakie filmy lubisz?"),
          selectInput("tytul", label=NULL, selected=c("Transformers: Wiek zaglady","Gladiator"), choices=sort(NazwyTytulow), multiple=TRUE),
         h4("Ile masz lat?"),
         radioButtons("wiek", label=NULL, choices=c("<18","18-29","30-44",">45","zawsze młody!")),
         h4("Jak określisz swoją płeć?"),
         radioButtons("plec", label=NULL, choices=c("kobieta","mężczyzna","nie określam")),
#          uiOutput("gatunek"),
#          uiOutput("reżyser"),
#          uiOutput("rok"),
            h4("Kryteria podobieństwa:"),
         checkboxGroupInput("kryteria", label=NULL, list(gatunek="Genres",
                                                         rok="Year",
                                                         reżyseria="Directedby",
                                                         obsada="Cast",
                                                         scenariusz="Writing",
                                                         kraje_produkcji="Production_countries",
                                                         słowa_kluczowe="Keywords",
                                                         tytuł="Title"),
                            list(gatunek="Genres")),
      actionButton("action", "Me Gusta!")
      ),
      mainPanel(
                  tabsetPanel(
                        tabPanel("Główna",
                                 br(),
                                 tableOutput("ranking")),
                                # plotOutput("barplot_ranking", width=800)),
                        tabPanel("HeatMapy podobieństwa",
                                 plotOutput("heatmap", width = 800)),
                        tabPanel("Mapa filmów", 
                                 plotOutput("mapa", width = 800)),
                        tabPanel("Instrukcja",
                                 br(),
                                 br(),
                                 br(),
                                          h5("Panel menu po lewej stronie służy do określenia kryteriów, 
którymi ma kierować się MEGUSTA podczas doboru filmów. Pierwsze pole, jest to pole tekstowe, 
służące do podania filmów, do których zostaną dobrane najbardziej podobne filmy z bazy 4000 filmów.
Dwie kolejne sekcje dotyczą wieku i płci użytkownika. Dzięki nim estrymator dobierze 
odpowiednie filtry z ocenami, tak by jego propozycja była lepiej dopasowana do osoby. W ostatniej
części znajdują się dodatkowe kryteria, którymi MEGUSTA ma się kierować przy doborze 
porównywanych filmów.", align="center"),
                                          br(),
                                          h5("W panelu głównym znajdują się trzy zakładki. W pierwszej 
znajduje się propozycja filmów, które użytkownik powinien obejrzeć.", align="center"),
                                          br(),
                                          h5("W dwóch kolejnych zakładkach jest wizualizacja graficzna
podobieństwa proponowanych filmów.", align="center"),
                                 br(),
                                 br()
                                 )
                               )
                  
      )
   )
)
)
