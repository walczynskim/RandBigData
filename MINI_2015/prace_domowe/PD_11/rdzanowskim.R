# install.packages("parallel")
library(parallel)
library(microbenchmark)

dluga <- function(){
  # wylosuj dane, 2 kolumny, 10000 wierszy
  df <- data.frame()
  for (i in 1:10000) {
    df <- rbind(df, data.frame(x=rnorm(1), y=rnorm(1)))
  }
  
  # policz modele regresji na probach bootstrapowych
  resx <- numeric()
  resy <- numeric()
  inda <- NULL
  
  for (i in 1:500) {
    ind <- sample(1:nrow(df),  replace = TRUE)
    resx[i] <- lm(x~y, data=df[ind,])$coef[1]
    resy[i] <- lm(x~y, data=df[ind,])$coef[2]
    inda <- rbind(inda, ind)
  }
  
  # posortuj wartosci w kazdym wierszu
  df2 <- cbind(resx, resy, inda)
  res <- apply(df2, 1, sort)
}

krotka <- function(){
  df <- data.frame(x = rnorm(10000), y = rnorm(10000))
  inda <- data.frame(matrix(sample(1:nrow(df), size = 500*nrow(df), replace = TRUE), ncol = 500))
  
  funkcja <- function(i, df, inda){
    return(lm(x~y, data = df[inda[[i]],])$coef)
  }
  
  rdzenie <- detectCores()
  cl <- makeCluster(rdzenie)
  res <- parSapply(cl, 1:500, funkcja, df, inda)
  stopCluster(cl)
  
  df2 <- cbind(t(res), t(inda))
  res <- apply(df2, 1, sort)
}

microbenchmark(dluga = dluga(), krotka = krotka(), times = 5)
