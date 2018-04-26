# load the dynamic regression library
library(dyn)

# number of observations
n <- 1000

# delta 
delta <- 1

#mu trend
mu <- 0.1

Time <- 0:n
x <- y <- r <- y <- t <- 0

plot(rep(0, n),type="l", xlim=c(1,n),ylim=c(-50, 200))

for (i in 1:10) {
	y <- rep(20, n)
	r <- rnorm(0:n)

	for (t in 2:(n+1)) {
		y[t] <- exp(mu + log(delta*y[t - 1]) + r[t])
		#y[t] <- log(h)
		if(t>n/2) {
			y[t] <- y[t] + 0
		}		
	}

	y <- ts(y)
	
	par(new=T)
	
	plot(log(y),type="l", xlim=c(1,n),ylim=c(-50, 200))
			
	c <- dyn$lm( y ~ lag(y, -1) )
			
	x[i] <- coef(summary(c))[2,1]
} 

#d <- density(x)
#plot(d)

xMean = mean(x)
xSD = sd(x)

#curve(dnorm(x, mean = 0, sd = xSD), add = T)

print(xMean)