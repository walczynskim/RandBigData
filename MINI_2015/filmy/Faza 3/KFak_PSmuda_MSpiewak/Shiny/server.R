library(shiny)
library(MASS)
setwd("~/Filmy/shiny/")
load("moviesDist.rda", envir = .GlobalEnv)
load("selectedTitles.rda", envir = .GlobalEnv)

shinyServer(function(input, output) {
      
      
      ########## FUNKCJA POMOCNICZA ##########
      moviesDistfun <- function(matricesNames, titles){
            z <- length(matricesNames)
            matricesList <- lapply(matricesNames, function(x) get(x, envir = .GlobalEnv))
            selectedTitles <- lapply(matricesList, function(macierz){
                  sapply( titles, function(tytul){
                        rownames(macierz) <- NazwyTytulow
                        macierz[tytul,]
                  })
            })
            names(selectedTitles) <- matricesNames
            n <- dim(selectedTitles[[1]])
            X <- array(unlist(selectedTitles), dim=c(n,z))
            distMatrix <- apply(X,c(1,2),function(x) mean(x, na.rm=TRUE))
            colnames(distMatrix) <- input$tytul
            # rownames(distMatrix) <- NazwyTytulow
            return(distMatrix)
            rm(matricesList,distMatrix,selectedTitles)
            gc()
      }
      
      czyscPamiec <- function(matrixName, sex){
            if(sex=="Female"){
                  matrixes <- c("Females_under_18","Females_Aged_18.29",
                                "Females_Aged_30.44","Females_Aged_45.")
                  matrixes <- matrixes[-which(matrixes==matrixName)]
                  suppressWarnings(rm(list = matrixes,envir = .GlobalEnv))
            } else if(sex=="Male"){
                  matrixes <- c("Males_under_18","Males_Aged_18.29",
                                "Males_Aged_30.44","Males_Aged_45.")
                  matrixes <- matrixes[-which(matrixes==matrixName)]
                  suppressWarnings(rm(list = matrixes,envir = .GlobalEnv))
            }
            gc()
      }
      
      ########################################
      
      zaladowaneMacierze <- reactive({
            input$action
            isolate({
                  path <- getwd()
                  message(path)
                  setwd("SimilarityMatrices/")
                  
                  # load("...") inne podstawowe macierze a następnie te opcjonalne
                  matricesNames <- c()
                  # załaduj macierze po personaliach
                  if(input$plec=="kobieta"){
                        if(input$wiek!="zawsze młody!"){
                              if(input$wiek=="<18"){
                                    load("Females_under_18.rda", envir = .GlobalEnv, verbose = TRUE)
                                    matricesNames <- c(matricesNames, "Females_under_18")
                                    czyscPamiec("Females_under_18","Female")
                              }else{
                                    if(input$wiek=="18-29"){
                                          load("Females_Aged_18.29.rda", envir = .GlobalEnv, verbose = TRUE)
                                          matricesNames <- c(matricesNames, "Females_Aged_18.29")
                                          czyscPamiec("Females_Aged_18.29","Female")
                                    }else{
                                          if(input$wiek=="30-44"){
                                                load("Females_Aged_30.44.rda", envir = .GlobalEnv, verbose = TRUE)
                                                matricesNames <- c(matricesNames, "Females_Aged_30.44")
                                                czyscPamiec("Females_Aged_30.44","Female")
                                          }else{
                                                if(input$wiek==">45"){
                                                      load("Females_Aged_45.rda", envir = .GlobalEnv, verbose = TRUE)
                                                      matricesNames <- c(matricesNames, "Females_Aged_45.")}
                                                czyscPamiec("Females_Aged_45.","Female")
                                          }
                                    }
                              }
                        }else{
                              load("Females.rda", envir = .GlobalEnv, verbose = TRUE)
                              matricesNames <- c(matricesNames, "Females")
                        }
                  }else{
                        if(input$plec=="mężczyzna"){
                              if(input$wiek!="zawsze młody!"){
                                    if(input$wiek=="<18"){
                                          load("Males_under_18.rda", envir = .GlobalEnv, verbose = TRUE)
                                          matricesNames <- c(matricesNames, "Males_under_18")
                                          czyscPamiec("Males_under_18","Male")
                                    }else{
                                          if(input$wiek=="18-29"){
                                                load("Males_Aged_18.29.rda", envir = .GlobalEnv, verbose = TRUE)
                                                matricesNames <- c(matricesNames, "Males_Aged_18.29")
                                                czyscPamiec("Males_Aged_18.29","Male")
                                          }else{
                                                if(input$wiek=="30-44"){
                                                      load("Males_Aged_30.44.rda", envir = .GlobalEnv, verbose = TRUE)
                                                      matricesNames <- c(matricesNames, "Males_Aged_30.44")
                                                      czyscPamiec("Males_Aged_30.44","Male")
                                                }else{
                                                      if(input$wiek==">45"){
                                                            load("Males_Aged_45.rda", envir = .GlobalEnv, verbose = TRUE)
                                                            matricesNames <- c(matricesNames, "Males_Aged_45.")}
                                                      czyscPamiec("Males_Aged_45.","Male")
                                                }
                                          }
                                    }
                              }else{
                                    load("Males.rda", envir = .GlobalEnv, verbose = TRUE)
                                    matricesNames <- c(matricesNames, "Males")
                              }
                        }else{ # else czyli "if = nieokreślam"
                              if(input$wiek!="zawsze młody!"){
                                    if(input$wiek=="<18"){
                                          load("Aged_under_18.rda", envir = .GlobalEnv, verbose = TRUE)
                                          matricesNames <- c(matricesNames, "Aged_under_18")
                                          czyscPamiec("Aged_under_18","Aged")
                                    }else{
                                          if(input$wiek=="18-29"){
                                                load("Aged_18.29.rda", envir = .GlobalEnv, verbose = TRUE)
                                                matricesNames <- c(matricesNames, "Aged_18.29")
                                                czyscPamiec("Aged_18.29","Aged")
                                          }else{
                                                if(input$wiek=="30-44"){
                                                      load("Aged_30.44.rda", envir = .GlobalEnv, verbose = TRUE)
                                                      matricesNames <- c(matricesNames, "Aged_30.44")
                                                      czyscPamiec("Aged_30.44","Aged")
                                                }else{
                                                      if(input$wiek==">45"){
                                                            load("Aged_45.rda", envir = .GlobalEnv, verbose = TRUE)
                                                            matricesNames <- c(matricesNames, "Aged_45.")}
                                                      czyscPamiec("Aged_45.","Aged")
                                                }
                                          }
                                    }
                              }else{
                                    load("Overall_Rating.rda", envir = .GlobalEnv, verbose=TRUE)
                                    matricesNames <- c(matricesNames, "Overall_Rating")
                              }
                        }
                  }
                  
                  # załaduj macierze po istotnych kryteriach
                  for( kryterio in input$kryteria){
                        nazwa <- paste0(kryterio,".rda")
                        load(nazwa, envir = .GlobalEnv, verbose = TRUE)
                  }
                  matricesNames <- c(matricesNames, input$kryteria)
                  setwd(path)
                  #              matricesList <- lapply(matricesNames,get)
                  return(matricesNames)
                  #  return(matricesList)
            })
            
      })
      
      
      policzDystans <- reactive({
            input$action
            isolate({
                  moviesDistfun(zaladowaneMacierze(), input$tytul)
            })
      })
      
      ###
      tytul <- reactive({
            input$action
            isolate({
                  a<-policzDystans()
                  n<-ncol(a)
                  if(n==1){
                        a<-cbind("Tytul filmu"=NazwyTytulow,a)
                        a<-a[order(a[,2],decreasing = TRUE),]
                        a<-head(a,20)
                        as.character(a[,1])
                  } else{
                        a<-cbind("Tytuł filmu"=NazwyTytulow,a)
                        b<-a[1:(n*20),]
                        for(i in 1:n){
                              c<-head(a[order(a[,i+1],decreasing = TRUE),],20)
                              b[((i-1)*20+1):(i*20),]<-c
                        }
                        b<-b[!duplicated(b[,1]),]
                        c<-b[,-1]
                        c<-apply(c,2,as.numeric)
                        d<-apply(c,1,sum)
                        b<-cbind(b,d)
                        b<-head(b[order(b[,ncol(b)],decreasing = TRUE),],20)[,-ncol(b)]
                        as.character(b[,1])
                  }
            })
      })
      
      output$ranking <- renderTable({
            input$action
            isolate({
                  a<-policzDystans()
                  n<-ncol(a)
                  if(n==1){
                        a<-cbind("Tytul filmu"=NazwyTytulow,a)
                        a<-a[order(a[,2],decreasing = TRUE),]
                        a<-head(a,20)
                        a[,2]<-round(as.numeric(a[,2]),4)
                        a
                  } else{
                        a<-cbind("Tytuł filmu"=NazwyTytulow,a)
                        b<-a[1:(n*20),]
                        for(i in 1:n){
                              c<-head(a[order(a[,i+1],decreasing = TRUE),],20)
                              b[((i-1)*20+1):(i*20),]<-c
                        }
                        b<-b[!duplicated(b[,1]),]
                        c<-b[,-1]
                        c<-apply(c,2,as.numeric)
                        d<-apply(c,1,sum)
                        b[,-1]<-apply(c,2,round,4)
                        b<-cbind(b,d)
                        b<-head(b[order(b[,ncol(b)],decreasing = TRUE),],20)[,-ncol(b)]
                        b
                  }
            })
      })
      
      MacierzPodobienstwa <- reactive({
            input$action
            isolate({
                  # tutaj filmy wybrane wyzej
                  filmy <- tytul()
                  #filmy <- NazwyTytulow[1:10]
                  wh <- unlist(lapply(filmy, function(x) which(NazwyTytulow == x)))
                  wh <- wh[1:20]
                  p <- moviesDist[wh, wh]/10
                  p
            })
      })
      
      output$heatmap <- renderPlot({
            input$action
            isolate({
                  #heatmap(policzDystans())
                  #filmy <- NazwyTytulow[1:10]
                  filmy <- tytul()
                  heatmap(MacierzPodobienstwa(), Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "darkorange"))(1000),
                          scale="none", margins=c(15, 10), labRow = filmy, labCol = filmy)
            })
      })
      
      output$mapa <- renderPlot({
            input$action
            isolate({
                  # barplot(daneAktor()[daneAktor()$Year==1995,"Duration"])
                  iso <- isoMDS(as.dist(MacierzPodobienstwa()))
                  x <- iso$points[,1]
                  y <- iso$points[,2]
                  plot(x, y, xlab = " ", ylab = " ",
                       xlim = range(x)*1.2, type = "n",  axes = FALSE, pch = 19)
                  text(x, y, labels = tytul(), lwd = 2, pch = 19)
                  box()
            })
      })
      
      
      
})