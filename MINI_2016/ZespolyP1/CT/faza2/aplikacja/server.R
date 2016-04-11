
library(ggplot2)
library(png)
library(shape)

#setwd("Uczelnia/R i Big Data/CNK/apka3/")


zaznacz.eksponat <- function(eks, wspX, wspY, cex,  ...) {
    points(wspX[eks], wspY[eks],  pch = 19, cex = cex, ...)
    text(wspX[eks], wspY[eks]+0.03, labels = eks, ...)
}


img <- readPNG("./plan.png")
load("pstwa.rda")
wsp <- read.csv("eksponaty2.csv")

wspX <- wsp$x
names(wspX) <- wsp$nazwa

wspY <- wsp$y
names(wspY) <- wsp$nazwa

#zwraca kilka najbardziej prawdopodobnych eksponatow (domyslnie 5)
getMax <- function(x, mat = pstwa, ile = 5){
    mat[x, head(order(mat[x,], decreasing = TRUE), ile)]
}

#zwraca sciezke generowana zgodnie z prawdopodobienstwami
randomPath <- function(first, l, mat = pstwa){
    sciezka <- vector("numeric", l)
    
    x = first
    sciezka[1] <- x
    
    mat[, x] <- -1
    
    for(i in 2 : l){
        y <- getMax(x, mat, 10)
        x = names(y)[sample(1:length(y), 1, prob = y)]
        mat[, x] <- -1
        sciezka[i] <- x
    }
    
    sciezka
}


shinyServer(function(input, output, session) {
    
    lim <- NULL
    cala_sciezka <- NULL
    
    tylkoWybranyeksponat <- reactive({
        sort(pstwa[input$wybranyEksponat, ], decreasing = TRUE)[1:input$suwak]
    })
    
    output$Tabela = renderDataTable({
        eksponat <- tylkoWybranyeksponat()
        
        tab <- data.frame(eksponat, names(eksponat))
        colnames(tab) <- c("pstwo", "nazwa")
        tab
    })
    
    output$wykres = renderPlot({
        eksponat <- tylkoWybranyeksponat()
        tab <- data.frame(eksponat, names(eksponat))
        colnames(tab) <- c("pstwo", "nazwa")
        pl <- ggplot(tab, aes(factor(nazwa, levels = nazwa), pstwo)) +
            geom_bar(stat = "identity") +
            xlab("eksponat") + 
            ylab("prawdopodobienstwo") +
            ggtitle(paste("Prawdopodobieństwa przejścia dla eksponatu", 
                          input$wybranyEksponat))
        pl
    })
    
    output$mapa <- renderPlot({
        wybrany <- input$wybranyEksponat
        bliskie <- names(sort(tylkoWybranyeksponat()))
        
        par(mar = c(0,0,0,0))
        plot(x=1:2, y=1:2, type='n', asp = 1)
        lim <<- par()
        rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
        
        wspX <- wspX / 663 * (lim$usr[2] - lim$usr[1]) + lim$usr[1]
        wspY <- wspY / 651 * (lim$usr[3] - lim$usr[4]) + lim$usr[4]
        
        
        for (b in seq_along(bliskie)) {
            if (bliskie[b] != wybrany)
                zaznacz.eksponat(bliskie[b], wspX, wspY, cex = 3*b/input$suwak)
            
        }
        
        
        zaznacz.eksponat(wybrany, wspX, wspY, cex = 3, col = "blue")
        
    })
    
    observeEvent(input$klik, {
        eksponaty <- tylkoWybranyeksponat()
        if (!is.null(input$klik)) {
            eks <- round(input$klik$x)
            eks <- names(eksponaty)[eks]
            updateSelectInput(session, "wybranyEksponat", selected = eks)
        }
    })
    
    observeEvent(input$klik_mapa, {
        eksponaty <- names(tylkoWybranyeksponat())
       

        wspX <- wspX / 663 * (lim$usr[2] - lim$usr[1]) + lim$usr[1]
        wspY <- wspY / 651 * (lim$usr[3] - lim$usr[4]) + lim$usr[4]

        
        df <- data.frame(x = wspX[eksponaty],
                         y = wspY[eksponaty])
        
        if (!is.null(input$klik_mapa)) {
            eks <- which.min(apply(df, 1, function(row) { 
                    (row[1] - input$klik_mapa$x)^2 + (row[2] - input$klik_mapa$y)^2
                }))
            eks <- eksponaty[eks]
       
            
            updateSelectInput(session, "wybranyEksponat", selected = eks)
        }
    })
    
    observeEvent(input$wybranyEksponat, {
        cala_sciezka <<- randomPath(input$wybranyEksponat, 45)
    })
    
    
    output$mapa2 <- renderPlot({
        wybrany <- input$wybranyEksponat
        poziom_suwaka <- input$suwak
        
        par(mar = c(0,0,0,0))
        plot(x=1:2, y=1:2, type='n', asp = 1)
        lim <<- par()
        rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
        
        wspX <- wspX / 663 * (lim$usr[2] - lim$usr[1]) + lim$usr[1]
        wspY <- wspY / 651 * (lim$usr[3] - lim$usr[4]) + lim$usr[4]
        
        sciezka <- cala_sciezka[1:poziom_suwaka]
        
        for (i in seq_along(sciezka)) {
            zaznacz.eksponat(sciezka[i], wspX, wspY, cex = 1)
        }
        
        x <- wspX[which(names(wspX) %in% sciezka)]
        ns <- factor(names(x), sciezka, ordered = TRUE)
        nsc <- as.integer(ns)
        x <- x[order(nsc)]
        y <- wspY[which(names(wspY) %in% sciezka)]
        y <- y[order(nsc)]
        
        arr.length <- 0.4
 
        
        Arrows(x[-length(sciezka)], 
               y[-length(sciezka)], 
               x[-1] +0.07 * (x[-length(sciezka)] - x[-1]), 
               y[-1] +0.07 * (y[-length(sciezka)] - y[-1]), 
               code = 2,
               arr.length = 0.3, 
               arr.width = arr.length/2, arr.adj = 0.5, arr.type = "curved",
               segment = TRUE)
        
    })
    
})


