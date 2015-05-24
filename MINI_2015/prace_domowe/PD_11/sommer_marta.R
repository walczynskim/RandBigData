library("microbenchmark")

biecek <- function(){
    
    df <- data.frame()
    for (i in 1:10000) {
      df <- rbind(df, data.frame(x=rnorm(1), y=rnorm(1)))
    }
    
    resx <- numeric()
    resy <- numeric()
    inda <- NULL
    
    for (i in 1:500) {
      ind <- sample(1:nrow(df),  replace = TRUE)
      resx[i] <- lm(x~y, data=df[ind,])$coef[1]
      resy[i] <- lm(x~y, data=df[ind,])$coef[2]
      inda <- rbind(inda, ind)
    }
    
    df2 <- cbind(resx, resy, inda)
    res <- apply(df2, 1, sort)
}

marta <- function(){
  
  ile <- 10000
  x <- rnorm(ile)
  y <- rnorm(ile)
  df <- data.frame(x, y)
  
  ile2 <- 500
  inda <- matrix(0, ncol=ile2, nrow=ile+2)
  inda <- data.frame(inda)
  for (i in 1:ile2) {
    ind <- sample(1:ile,  replace = TRUE)
    inda[1:2, i] <- lm(x~y, data=df[ind,])$coef
    inda[3:(ile+2), i] <- ind
  }  
  res <- apply(inda, 2, sort)
}

microbenchmark(biecek = {biecek()},  marta = {marta()}, times=1)



