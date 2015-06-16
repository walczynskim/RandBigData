library(shiny)
library(projektFilmy)
library(stringi)

# wykonywane raz gdy uruchamiana jest plikacja


shinyServer(function(input, output) {
  dane <- projektFilmy::filmy[1:500,]
  # odpowiedź na kontrolki 
  
  output$instrukcja <- renderText({
    "1) Wybierz jaki film z listy Cię interesuje.\n\n2) Wybierz, ile filmów chcesz obejrzeć.\n\n3) Wybierz wagi w poszczególnych kategoriach zwracając największą uwagę na te parametry, które Cię najbardziej interesują.\n\n\nI voila!\n\n\n4) W zakładce TABELA znajdziesz listę filmów najbardziej podobnych do filmu wybranego przez Ciebie.\n\n5) W zakładce WYKRES 2D znajdziesz wykres ilustrujący to podobieństwo.\n\n6) W zakładce WYKRES 3D znajdziesz podobny, lecz trójwymiarowy wykres.\n\n7) W zakładce WYKRES 3D (obracalny) znajdziesz trójwymiarowy i obracalny wykres ilustrujący podobieństwo między filmami. Wykres tworzy się w nowym oknie!"
  })
  
  output$tabela <- renderTable({
    if(input$czas+input$ocena+input$widocznosc+input$geo+input$naj == 0){return(NULL)}
    dist_pod <- dist_end(dane = dane, which(dane$tytul_filmweb == input$film), 
                         wagi = c(input$czas, input$ocena, input$widocznosc, input$geo, input$naj))
    dist_niepod <- matrix(rep(1, nrow(dane)), ncol = 1) - dist_pod
    pod <- podobne_filmy_2(dist_pod, wybrane = 1, ile = input$ile)
    niepod <- podobne_filmy_2(dist_niepod, ile = input$ile)
    
#     stresz <- dane$filmweb_krotki_opis_filmu
#     pod_stresz <- stresz[which(dane$tytul_filmweb %in% pod)][1:input$ile]
#     niepod_stresz <- stresz[which(dane$tytul_filmweb %in% niepod)][1:input$ile]
#     
#     jakie <- unique(c(as.character(pod), as.character(niepod), colnames(dist_pod)))
#     w <- which(dane$tytul_filmweb %in% jakie)
#     
#     dist_filmy <- t(dist_end(dane = dane, wybrane = w, 
#                              wagi = c(input$czas, input$ocena, input$widocznosc, input$geo, input$naj)))    
#     
    
    kolumna1 <- paste(input$ile, "najbardziej podobnych filmów do filmu", input$film)
    kolumna2 <- paste("Streszczenia", input$ile, "najbardziej podobnych filmów do filmu", input$film)
    kolumna3 <- paste(input$ile, "najmniej podobnych filmów do filmu", input$film)
    kolumna4 <- paste("Streszczenia", input$ile, "najmniej podobnych filmów do filmu", input$film)
    #ramka <- data.frame(pod, pod_stresz, niepod, niepod_stresz)
    ramka <- data.frame(pod$wynik, dane$filmweb_krotki_opis_filmu[pod$ktore], 
                        niepod$wynik, dane$filmweb_krotki_opis_filmu[niepod$ktore])
    colnames(ramka) <- c(kolumna1, kolumna2, kolumna3, kolumna4)    
    return(ramka)
  })

  output$wykres2d <- renderPlot({
    if(input$czas+input$ocena+input$widocznosc+input$geo+input$naj == 0){return(NULL)}
    dist_pod <- dist_end(dane = dane, which(dane$tytul_filmweb == input$film), 
                         wagi = c(input$czas, input$ocena, input$widocznosc, input$geo, input$naj))
    dist_niepod <- matrix(rep(1, nrow(dane)), ncol = 1) - dist_pod
    pod <- podobne_filmy_2(dist_pod, ile = input$ile)$wynik
    niepod <- podobne_filmy_2(dist_niepod, ile = input$ile)$wynik
    
    jakie <- unique(c(as.character(pod), as.character(niepod), colnames(dist_pod)))
    w <- which(dane$tytul_filmweb %in% jakie)
    
    dist_filmy <- t(dist_end(dane = dane, wybrane = w, 
                             wagi = c(input$czas, input$ocena, input$widocznosc, input$geo, input$naj)))    
    skalowanie(dist = dist_filmy, dim = 2, rgl = FALSE)
  })
  
  output$wykres3d <- renderPlot({
    if(input$czas+input$ocena+input$widocznosc+input$geo+input$naj == 0){return(NULL)}
    dist_pod <- dist_end(dane = dane, which(dane$tytul_filmweb == input$film), 
                         wagi = c(input$czas, input$ocena, input$widocznosc, input$geo, input$naj))
    dist_niepod <- matrix(rep(1, nrow(dane)), ncol = 1) - dist_pod
    pod <- podobne_filmy_2(dist_pod, ile = input$ile)$wynik
    niepod <- podobne_filmy_2(dist_niepod, ile = input$ile)$wynik
    
    jakie <- unique(c(as.character(pod), as.character(niepod), colnames(dist_pod)))
    w <- which(dane$tytul_filmweb %in% jakie)
    
    dist_filmy <- t(dist_end(dane = dane, wybrane = w, 
                             wagi = c(input$czas, input$ocena, input$widocznosc, input$geo, input$naj)))    
    skalowanie(dist = dist_filmy, dim = 3, rgl = FALSE)
  })
  
  output$wykres3d_nowy <- renderPlot({
    if(input$czas+input$ocena+input$widocznosc+input$geo+input$naj == 0){return(NULL)}
    dist_pod <- dist_end(dane = dane, which(dane$tytul_filmweb == input$film), 
                         wagi = c(input$czas, input$ocena, input$widocznosc, input$geo, input$naj))
    dist_niepod <- matrix(rep(1, nrow(dane)), ncol = 1) - dist_pod
    pod <- podobne_filmy_2(dist_pod, ile = input$ile)$wynik
    niepod <- podobne_filmy_2(dist_niepod, ile = input$ile)$wynik
    
    jakie <- unique(c(as.character(pod), as.character(niepod), colnames(dist_pod)))
    w <- which(dane$tytul_filmweb %in% jakie)
    
    dist_filmy <- t(dist_end(dane = dane, wybrane = w, 
                             wagi = c(input$czas, input$ocena, input$widocznosc, input$geo, input$naj)))    
    colnames(dist_filmy) <- stri_replace_all_regex(colnames(dist_filmy), "ą|ć|ę|ł|ń|ó|ś|ź|ż|Ą|Ć|Ę|Ł|Ń|Ó|Ś|Ź|Ż", "_")
    rownames(dist_filmy) <- stri_replace_all_regex(rownames(dist_filmy), "ą|ć|ę|ł|ń|ó|ś|ź|ż|Ą|Ć|Ę|Ł|Ń|Ó|Ś|Ź|Ż", "_")
    skalowanie(dist = dist_filmy, dim = 3, rgl = TRUE)
  })

})

