# load the dynamic regression library
library(dyn)

# number of observations
n <- 10000

# alpha intercept
alpha <- 0

# delta variance of the error term
delta <- 100

Time <- 0:n
x <- y <- r <- y <- t <- 0

L1Y <- 0.5
L2Y <- 0
L3Y <- 0

LE1 <- 10
LE2 <- 0
LE3 <- 0

r <- rnorm(1:n)*delta

y <- rep(0, n)

# delta variance of the error term
sigmaU <- 0
u <- rnorm(1:n)*sigmaU

for (t in 2:n) {

	# ARMA(1, 1)
  y[t] <- alpha + L1Y*y[t - 1] + LE1*r[t - 1] + r[t]		

	# ARMA(2, 1)
	#y[t] <- alpha + L1Y*y[t - 1] + L2Y*y[t - 2] + LE1*r[t - 1] + r[t]	

	# ARMA(2, 2)
	#y[t] <- alpha + L1Y*y[t - 1] + L2Y*y[t - 2] + LE1*r[t - 1] + LE2*r[t - 2] + r[t]		
	# ARMA(1, 2)
	#y[t] <- alpha + L1Y*y[t - 1] + LE1*r[t - 1] + LE2*r[t - 2] + LE3*r[t-3] + r[t]	
	
	# ARMA(3, 1)
	#y[t] <- alpha + L1Y*y[t - 1] + L2Y*y[t - 2] + L3Y*y[t - 3] + LE1*r[t - 1] + r[t]	

	# ARMA(3, 2)
	#y[t] <- alpha + L1Y*y[t - 1] + L2Y*y[t - 2] + L3Y*y[t - 3] + LE1*r[t - 1] + LE2*r[t - 2] + r[t]	

	# ARMA(1, 3)
	#y[t] <- alpha + L1Y*y[t - 1] + LE1*r[t - 1] + LE2*r[t - 2] + LE3*r[t - 3] + r[t]	

	# ARMA(2, 3)
	#y[t] <- alpha + L1Y*y[t - 1] + L2Y*y[t - 2] + LE1*r[t - 1] + LE2*r[t - 2] + LE3*r[t - 3] + r[t]	
			
	# ARMA(3, 3)
	#y[t] <- alpha + L1Y*y[t - 1] + L2Y*y[t - 2] + L3Y*y[t - 3] + LE1*r[t - 1] + LE2*r[t - 2] + LE3*r[t - 3] + r[t]	
	
	}
	
	y <- ts(y)
	
	#c <- dyn$lm( y ~ lag(y, -1) )	
	#c <- dyn$lm( y ~ lag(y, -1) + lag(y, -2) + lag(y, -3) )	
	#c <- dyn$lm( y ~ lag(y, -1) + lag(y, -2) + lag(y, -3) )	
	
	#x[i] <- coef(summary(c))[2,1]
	
	#print(summary(c))
	#plot(y, type="l")
	#readline("Press return")
#}

#plot(y, type="l")
#x[i] <- coef(summary(c))[2,1]

#print(summary(x))
#print(mean(x))
	
y <- y + u
	
acf(y)
 
arima(y, order = c(1,0,1))



