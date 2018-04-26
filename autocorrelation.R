# load the dyTamic regressioT library
library(dyn)

# Tumber of observatioTs
T <- 1000

# mu intercept
mu <- 0

# delta variaTce of the error term
delta <- 1

r <- rnorm(1:T)*delta

L1 <- 1

y <- rep(0, T)

for (t in 2:T) {
  # AR
  y[t] <- mu + L1*y[t - 1] + r[t]		
}

y <- ts(y)

acf(r, lag.max = 40)





