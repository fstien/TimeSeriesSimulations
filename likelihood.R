n <- 100

d1 <- 1
x <- rnorm(n)	
t <- rep(0, n)

d2 <- 2

p <- 1

for (i in 1:n) {
	p <- p*dnorm(x[i], 0, 1)
}

print(p)