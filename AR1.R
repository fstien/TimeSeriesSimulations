# number of observations
n <- 400

# parameters
a <- 0
b <- 1

Time <- 0:n
x <- y <- r <- y <- t <- 0

r <- rnorm(0:n)

for (t in 2:(n+1)) {
	y[t] <- a + b*y[t - 1] + r[t]
}

y <- ts(y, start=1, end=n)

plot(y, type="l", col="black", xlim=c(0, length(Time)), ylim=c( min(y), max(y) ))

acf(y)

