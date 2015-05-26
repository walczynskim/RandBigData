# 3krotne poprawienie predkosci:

system.time({
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
   
})
# użytkownik     system   upłynęło 
# 40.23       0.97      41.53 

library(dplyr)
system.time({
   df <- data.frame(x=rnorm(10^4), y=rnorm(10^4))
   
   indexes <- replicate(500, sample(1:(10^4),  replace = TRUE))
   
   cbind(indexes,
      t(apply(indexes, 1, function(element){
      lm(x~y, data=df[element, ])$coeff[1:2]
   })
      )
   ) -> df2
   res <- apply(df2, 1, sort)
})

# użytkownik     system   upłynęło 
# 14.21       0.04      14.31 
