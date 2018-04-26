# load the dynamic regression library
library(dyn)

# number of observations
n <- 100

# delta 
delta <- 1

Time <- 0:n
x <- y <- r <- y <- t <- 0

for (i in 1:10) {
	y <- r <- rnorm(0:n)


	for (t in 2:(n+1)) {
		y[t] <- delta*y[t - 1] + r[t]
		if(t>n/2) {
			y[t] <- y[t] + 0
		}		
	}
  

	y <- ts(y)
			
	c <- dyn$lm( y ~ lag(y, -1) )
			
	x[i] <- coef(summary(c))[2,1]
} 

d <- density(x)
plot(d)

xMean = mean(x)
xSD = sd(x)

#curve(dnorm(x, mean = 0, sd = xSD), add = T)

print(xMean)