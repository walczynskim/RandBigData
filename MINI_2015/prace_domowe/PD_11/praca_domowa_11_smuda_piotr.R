funkcja_przed<-function(){
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

funkcja_po<-function(){
   # wylosuj dane, 2 kolumny, 10000 wierszy
   df <- data.frame(x=rnorm(10000), y=rnorm(10000))
   n <- 10000

   # policz modele regresji na probach bootstrapowych
   inda <- lapply(1:500, function(i){
      ind <- sample(1:n, replace=TRUE)
      model <- lm(x~y, data=df[ind,])
      inda <- c(model$coefficients, ind)
   })

   # posortuj wartosci w kazdym wierszu
   df2 <- do.call("rbind", inda)
   res <- apply(df2, 1, sort, method="quick")
}

microbenchmark::microbenchmark(
   funkcja_po(),
   funkcja_przed(),
   unit='relative',
   times=5
)
