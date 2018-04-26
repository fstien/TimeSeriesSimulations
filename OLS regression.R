# coefficient
beta <- 0.2

# intercept
alpha <- 2

# standard deviation error term
delta <- 1

# number of observations generated
n <- 200

x <- y <- r <- y <- t <- 0

for(z in 1:1000) {
	r <- rnorm(1:n)*delta
	x <- 1:n
		
	for(i in 0:n) { 
		y[i] = alpha + beta*i
	}
	
	y <- y + r
	c <- lm(y ~ x)	
	t[z] <-coef(summary(c))[2,1]
} 

print(mean(t))