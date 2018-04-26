n <- 50
T <- 80
P <- 10
Qvec <- rep(0, n)

for (t in 1:n) {
  r <- rnorm(1:T)
  Q <- 0
  
  for(i in 1:P) { 
    Q <- Q + (acf(r)[[1]][i])^2
  }
  
  Q <- T*Q
  Qvec[t] <- Q
}

plot(density(Qvec))


