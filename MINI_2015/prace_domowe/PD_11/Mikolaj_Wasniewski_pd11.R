# Mikolaj Wasniewski 
# praca domowa 11 


f1<-function(){
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


## moje rozwi¹zanie 
f2 <- function(){
   n<-10000
   df<-data.frame(x=rnorm(n), y=rnorm(n))
   
   df2<-sapply(1:500 ,function(x){
      ind <- sample(1:n,  replace = TRUE)
      df1<-df[ind,]
      res <- lm(x~y, data=df[ind,])$coef
      sort(c(res,ind))
   })
   df2
}


# szybsze rozwi¹zanie 
f3 <- function(){
   n<-10000
   df<-data.frame(x=rnorm(n), y=rnorm(n))
   
   inda<-vapply(1:500 ,function(x){
      ind <- sample(1:n,  replace = TRUE)
      df1<-df[ind,]
      res <- lm.fit(matrix(c(rep(1, n), df1$y), ncol=2), df1$x)$coef
      sort(c(res,ind))
   },numeric(n+2))
   inda
}



library(microbenchmark)


microbenchmark(f1(), f2(), f3(),times=20L, unit = "relative")
# Unit: relative
# expr      min       lq     mean   median       uq      max neval cld
# f1() 8.668658 8.459764 8.228641 8.451321 8.074202 7.700668    20   c
# f2() 1.943851 1.925104 1.983444 1.927795 2.107856 1.963926    20  b 
# f3() 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000    20 a