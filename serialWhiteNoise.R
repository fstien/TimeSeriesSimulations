n <- 1000

T <- 100

acfVec <- rep(0, n)

for (t in 1:n) {
  
  r <- rnorm(1:T)
  
  acfVec[t] <- sqrt(T)*acf(r)[2][[1]]
  
}

plot(density(acfVec))


