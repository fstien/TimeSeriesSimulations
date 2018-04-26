# load the dynamic regression library
library(dyn)

# number of observations
n <- 10000

# delta 
delta <- 2
mu <- 0

Time <- 0:n
x <- y <- r <- y <- t <- 0

for (i in 1:10) {
	y <- r <- rnorm(0:n)

	for (t in 2:(n+1)) {
		y[t] <- mu + delta*r[t - 1] + r[t]
		
		# to add a structural change
		if(t>n/2) {
			y[t] <- y[t] + 0
		}
	}

	y <- ts(y)
	
	
	c <- dyn$lm( y ~ lag(y, -1) )
			
	#x[i] <- coef(summary(c))[2,1]
  x[i] <- coef(arima(y, order = c(1,0,0)))[1]
} 

pacf(y)



# d < - density(x)
# plot(d)
# xMean = mean(x)
# xSD = sd(x)
# curve(dnorm(x, mean = xMean, sd = xSD), add = T)
# print(xMean)