library(dplyr)
system.time({
  df <- data.frame(x=rnorm(10^4), y=rnorm(10^4))
 
  indexes <- replicate(500, sample(1:(10^4),  replace = TRUE))

  xy <- t(apply(indexes, 2, function(element){
          data <- df[element, ]
          beta <- cov(data$x,data$y)/var(data$x)
          c(mean(data$y)-beta * mean(data$x), beta)
        })
        )
  df2 <- cbind(xy, t(indexes))

  res <- apply(df2, 1, sort, method = "quick")
})
