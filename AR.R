# load the dynamic regression library
library(dyn)

# number of observations
n <- 400

# delta 
delta <- 1
mu <- 0

Time <- 0:n
x <- y <- r <- y <- t <- 0

for (i in 1:10) {
	y <- r <- rnorm(0:n)

	for (t in 2:(n+1)) {
		y[t] <- mu + delta*y[t - 1] + r[t]
		if(t>n/2) {
			y[t] <- y[t] + 0
		}
	}

	y <- ts(y, start=1, end=n)
			
	reg <- dyn$lm( y ~ lag(y, -1) )

	res <- residuals(reg)
	res <- ts(res, start=1, end=40)

	pred <- predict(reg) 
	pred <- ts(pred, start=1, end=40)

} 

plot(y, type="l", col="black", xlim=c(0, length(Time)), ylim=c( min(y), max(y) ))

acf(y)

par(new=T)
plot(pred, type="l", col="blue", xlim=c(0, length(Time)), ylim=c( min(y), max(y) ))
# d < - density(x)
# plot(d)
# xMean = mean(x)
# xSD = sd(x)
# curve(dnorm(x, mean = xMean, sd = xSD), add = T)
# print(xMean)