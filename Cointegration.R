# load the dynamic regression library
library(dyn)

# number of observations
n <- 10000

# delta 
delta1 <- 0.95
delta2 <- 0.85

Time <- 0:n

yI1 <- yI2 <- yS1 <- yS2 <- rep(0, n)

r1 <- rnorm(1:n)
r2 <- rnorm(1:n)
r3 <- rnorm(1:n)

for (t in 2:n) {
	yS1[t] <- delta1*yS1[t - 1] + r1[t]	
}

for (t in 2:n) {
	yS2[t] <- delta2*yS2[t - 1] + r2[t]	
}

for (t in 2:n) {
	yI1[t] <- yI1[t - 1] + r3[t]
}

yI2 <- 1.7*yI1

yI1 <- yI1 + yS1

yI2 <- yI2 + yS2

yI1 <- ts(yI1)
yI2 <- ts(yI2)
					

plot(1:n,yI1,type="l",col="red", xlim=c(1,n),ylim=c(min(yI2), max(yI2)))
par(new=T)
plot(1:n,yI2, type="l", col="blue",xlim=c(1,n),ylim=c(min(yI2), max(yI2)))
par(new=T)
plot(1:n,yS1, type="l", col="black",xlim=c(1,n),ylim=c(min(yI2), max(yI2)))


