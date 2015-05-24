f1 <- function(){
  df <- data.frame(x=rnorm(10000), y=rnorm(10000))
  pikus <- replicate(500,
                         {
                           ind <- sample(1:10000,  replace = TRUE)
                           # lm_temp <- lm(x~y, data=df[ind,])$coef
                           c(lm(x~y, data=df[ind,])$coef, ind)
                         }, simplify = FALSE)
  df2 <-  do.call(rbind, pikus)
  res <- apply(df2, 1, FUN= function(x) {sort(x, method='quick')})
}

f2 <- function(){
  df <- data.frame(x=rnorm(10000), y=rnorm(10000))

  pikus <- lapply(1:500,FUN=function(x)
                     {
                       ind <- sample(1:10000,  replace = TRUE)
                       c(lm(x~y, data=df[ind,])$coef, ind)
                     })
  df2 <-  do.call(rbind, pikus)
  res <- apply(df2, 1, FUN= function(x) {sort(x, method='quick')})
}

require(data.table)
f3 <- function(){
  df <- data.frame(x=rnorm(10000), y=rnorm(10000))
  pikus <- lapply(1:500,FUN=function(x)
  {
    ind <- sample(1: 10000,  replace = TRUE)
    c(lm(x~y, data=df[ind,])$coef, ind)
  })
  dt1 <- data.table(do.call(cbind, pikus))
  res2 <- as.matrix(dt1[,lapply(.SD, sort)])
}

  
system.time(f1())

system.time(f2())

system.time(f3())

microbenchmark::microbenchmark(f1(),f2(),f3(), times = 100, unit="relative")
# > system.time(f1())
# user  system elapsed 
# 5.74    0.03    5.77 
# > # > system.time(f1())
#   > # user  system elapsed 
#   > # 5.27    0.06    5.36
#   > system.time(f2())
# user  system elapsed 
# 5.59    0.04    5.64 
# > # > system.time(f2())
#   > # user  system elapsed 
#   > # 5.27    0.03    5.43 
#   > system.time(f3())
# user  system elapsed 
# 5.43    0.00    5.44 
# > # > system.time(f3())
#   > # user  system elapsed 
#   > # 5.12    0.00    5.16
#   > 
#   > microbenchmark::microbenchmark(f1(),f2(),f3(), times = 100, unit="relative")
# Unit: relative
# expr      min       lq     mean   median       uq      max neval cld
# f1() 1.043390 1.056584 1.061572 1.066130 1.061478 1.095324   100   b
# f2() 1.055984 1.052238 1.052147 1.053533 1.052575 1.050064   100   b
# f3() 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100  a 
