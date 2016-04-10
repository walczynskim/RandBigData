weekdays <- as.list(c(2:7))
names(weekdays) <- c("Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")

weekdays_choices <- as.list(c(1:4))
names(weekdays_choices) <- c("Cały tydzień", "Wtorek - Piątek", "Sobota - Niedziela", "Custom")

months_choices <- as.list(c(1:3))
names(months_choices) <- c("Cały rok", "Wakacje (Lipiec - Sierpień)", "Custom")

data(dane_all)
data("slownik_urz")
data("mapka_png")
rownames(slownik_urz) <- slownik_urz$nr
slownik_urz %>% filter(x != 0) -> slownik_urz

shinyServer(function(input, output, session) {
  
  AppType <- reactive({
    print("a")
    input$AppType
  })
  
  selectedWeekDays <- reactive({
    if(AppType() == 2){
      if(input$dzienTygodnia == 1) {return(2:7)}
      if(input$dzienTygodnia == 2) {return(2:5)}
      if(input$dzienTygodnia == 3) {return(6:7)}
      if(input$dzienTygodnia == 4) {
        return(input$dzienTygodniaCustom)}
    }
    else {return(2:7)}
  })
  
  selectedMonths <- reactive({
    print("c")
    if(AppType() == 2){
      if(input$miesiac == 1) {return(1:12)}
      if(input$miesiac == 2) {return(7:8)}
      if(input$miesiac == 3) {return(input$miesiacCustom[1]:input$miesiacCustom[2])}
    }
    else {return(1:12)}
  })
  
  selectedPathLength <- reactive({
    input$czySciezka
  })
  
  output$SelektorDni <- renderUI({
    if(AppType() == 1){
      dateRangeInput(inputId = "dzien", 
                  label = "Wybierz zakres dni",
                  start = "2013-01-01",
                  end = "2013-12-31",
                  min = "2013-01-01",
                  max = "2013-12-31",
                  weekstart = 1,
                  language = "pl",
                  separator = " do ")
    }
  })
  
  output$SelektorDniaTygodnia <- renderUI({
    if(AppType() == 2){
      selectInput(inputId = "dzienTygodnia", 
                  label = "Wybierz zakres dni tygodnia",
                  choices = weekdays_choices,
                  selected = 1)
    }
  })
  
  output$SelektorDniaTygodniaCustom <- renderUI({
    if(AppType() == 2){
      if(input$dzienTygodnia == 4){
        checkboxGroupInput(inputId = "dzienTygodniaCustom", 
                    label = "Wybierz dni tygodnia",
                    choices = weekdays,
                    selected = 2:7)
      }
    }
  })
  
  output$SelektorMiesiaca <- renderUI({
    if(AppType() == 2){
      selectInput(inputId = "miesiac", 
                  label = "Wybierz zakres miesięcy",
                  choices = months_choices,
                  selected = 1)
    }
  })
  
  output$SelektorMiesiacaCustom <- renderUI({
    if(AppType() == 2){
      if(input$miesiac == 3){
        sliderInput(inputId = "miesiacCustom", 
                    label = "Wybierz miesiące",
                    min=1,
                    max = 12,
                    value = c(1,12),
                    step = 1,
                    round = TRUE)
      }
    }
  })
  
  
  output$SelektorDlugosciSciezki <- renderUI({
    if(selectedPathLength()) {
      sliderInput(inputId = "dlugoscSciezki", 
                  label = "Wybierz długość ścieżki",
                  min = 7,
                  max = 24,
                  value = 10,
                  step = 1,
                  round = TRUE)
    }
  })
  
  
  output$text <- renderUI({
    str1 <- findClosest(c(input$plot_hover$x, 886 - input$plot_hover$y), slownik_urz)[1]
    str2 <- findClosest(c(input$plot_hover$x, 886 - input$plot_hover$y), slownik_urz)[2]
    HTML(paste(str1, str2, sep = '<br/>'))
    
  })
  
  output$sciezki <- renderPlot({
    print(input$dzienTygodniaCustom)
    daty <- seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), by = 1)
    if(AppType() == 2){
      filtr <- filter_data(weekdays = selectedWeekDays(), months = selectedMonths(), hours = input$godzina)
    }
    if(AppType() == 1){
      if(input$dzien[2]>= input$dzien[1]) {
        filtr <- filter_data_byday(days =input$dzien ,hours = input$godzina)
      }
      else filtr <- filter_data_byday(days = rev(input$dzien), hours = input$godzina)
    }
    filtr$from <- as.character(filtr$from)
    filtr$to <- as.character(filtr$to)
    sciezka <- sciezka(filtr, input$dlugoscSciezki)
    sciezka <- sciezka %>% filter(from %in% slownik_urz$nr, to %in% slownik_urz$nr)
    if(max(filtr$total)!=0){
      filtr$total <- filtr$total / sort(filtr$total, decreasing = T)[8]
    }
    filtr$total[filtr$total>1] <- 1
    filtr$total <- filtr$total^1.3
    print(sort(filtr$total, decreasing = T)[1:10])
    rozmiary <- plot_mapa(mapka_png, obram=F)
    plot_paths(filtr, slownik_urz, col = "#160773")
    if(selectedPathLength()){
      plot_polaczenia_graph(data = sciezka , 
                            slownik = slownik_urz, 
                            alpha = 0.5, 
                            czyStrzalki = T, 
                            szerStrzalek = 2, 
                            kolLinii = "#bf1010", 
                            rozmiarStrzalek = 1 )
    }
    plot_urzadz(slownik_urz, col = "#3a2e85", cex=3.7)
    plot_etykiety_nr(slownik_urz, przes= c(0,0), cex = 0.9)
  }, height = 900, width = 800)
  
  
})