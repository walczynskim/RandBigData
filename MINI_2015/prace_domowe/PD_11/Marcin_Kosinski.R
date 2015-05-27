library(dplyr)
system.time({
	df <- data.frame(x=rnorm(10^4), y=rnorm(10^4))
	
	indexes <- replicate(500, sample(1:(10^4),  replace = TRUE))
	
	cbind(indexes,
				t(apply(indexes, 1, function(element){
					data <- df[element, ]
					beta <- cov(data$x,data$y)/var(data$x)
					c(mean(data$y)-beta * mean(data$x), beta)
				})
				)
	) -> df2
	res <- apply(df2, 1, sort, method = "quick")
})
# 
# user  system elapsed 
# 4.529   0.034   4.561 

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
# 
# user  system elapsed 
# 32.379   0.128  32.486 
