zrobcos <- function() {
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


############################################################################
# przyspieszenie czterokrotne
############################################################################

zrobcos2 <- function() {
   df <- data.frame(x=rnorm(10000), y=rnorm(10000))
   resx <- numeric(500)
   resy <- numeric(500)
   inda <- vector("list", 500)
   for(i in 1:500) {
      ind <- sample(1:10000,  replace = TRUE)
      resx <- lm(x~y, data=df[ind,])$coef[1]
      resy <- lm(x~y, data=df[ind,])$coef[2]
      inda[[i]] <- sort(c(resx, resy, ind))
   }

   res <- t(matrix(unlist(inda), nrow = 500, byrow=TRUE))
}


############################################################################
# przyspieszenie ośmiokrotne
############################################################################

zrobcos3 <- function() {
   df <- data.frame(x=rnorm(10000), y=rnorm(10000))
   resx <- numeric(500)
   resy <- numeric(500)
   inda <- vector("list", 500)
   for(i in 1:500) {
      ind <- sample(1:10000,  replace = TRUE)
      model <- lm(x~y, data=df[ind,])
      resx <- model$coef[1]
      resy <- model$coef[2]
      inda[[i]] <- sort(c(resx, resy, ind))
   }
   res <- t(do.call("rbind", inda))
}

microbenchmark::microbenchmark(zrobcos(), zrobcos2(), zrobcos3(), times = 2, unit = "relative")
# Unit: relative
# expr      min       lq     mean   median       uq      max neval cld
# zrobcos() 7.065135 7.065135 7.117782 7.117782 7.169879 7.169879     2   c
# zrobcos2() 1.873541 1.873541 1.864581 1.864581 1.855715 1.855715     2  b 
# zrobcos3() 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000     2 a 