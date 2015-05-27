########### FUNKCJA STARA #########################

funkcja1 <- function(){
  
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

############ MOJA FUNKCJA ############################

funkcja2 <- function(){
  
  df <- data.frame(x=rnorm(10000), y=rnorm(10000)) 
  
  inda <- matrix(0, nrow=500, ncol=10000+2)  
  
  for(i in 1:500){
    
    ind <- sample(1:10000,  replace = TRUE)
    inda[i,] <- sort(c(lm(y~x, data = df[ind,])$coef,ind))
  }
  
}
