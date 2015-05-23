######################################
### Praca domowa 11
### Martyna Spiewak
######################################


# podstawowa wersja
f1 <- function(){
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

# ulepszona wersja
f2 <- function(){
  # wylosuj dane, 2 kolumny, 10000 wierszy
  df <- data.frame(x = rnorm(10000), y = rnorm(10000))

  # policz modele regresji na probach bootstrapowych
  inda <- vector("list", 500)
    
  for (i in 1:500) {
    ind <- sample(1:nrow(df),  replace = TRUE)
    l <- lm(x~y, data=df[ind,])$coef
    resx <- as.numeric(l[1])
    resy <- as.numeric(l[2])
    inda[[i]] <- c(resx = resx, resy = resy, ind)
  }

  # posortuj wartosci w kazdym wierszu
  df2 <- do.call("rbind", inda)
  res <- apply(df2, 1, sort)
}


microbenchmark::microbenchmark(f1(), f2(), times = 1, unit="relative")
# Unit: relative
# expr      min       lq     mean   median       uq      max neval
# f1() 10.70259 10.70259 10.70259 10.70259 10.70259 10.70259     1
# f2()  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000     1

# okolo 10 razy szybciej
