PracaDomowa <- function(){
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

  res
}


MojaFunkcja <- function(){
  # wylosuj dane, 2 kolumny, 10000 wierszy
  df <- data.frame(x=rnorm(10000,0,1), y=rnorm(10000,0,1))
  
  # policz modele regresji na probach bootstrapowych
  n <- nrow(df)
  tmp <- matrix(numeric(500*(n+2)), nrow=500)
  
  for(i in 1:500){
    ind <- sample(sample(1:n,  replace = TRUE))
    l <- lm(x~y, data=df[ind,])
    tmp[i, ] <- sort(c(l$coef[1], l$coef[2], ind))
  }
  
  tmp
}



microbenchmark::microbenchmark(MojaFunkcja(),
                               PracaDomowa(),
                               fff(), times=20)
# Unit: seconds
# expr              min       lq     mean   median       uq       max neval cld
# MojaFunkcja() 11.87999 12.00571 12.36946 12.14481 12.32939  16.27885    20 a  
# PracaDomowa() 95.97705 96.71630 97.95584 97.42717 99.37689 101.27027    20   c