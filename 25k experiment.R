# load the dynamic regression library
library(dyn)

# number of observations
n <- 100

# delta 
delta <- 0.95
Time <- 0:n

z <- 0
count <- 0
runs <- 500

for (i in 1:runs) { 
	
	y1 <- y2 <- rep(0, n)

	r1 <- rnorm(1:n)
	r2 <- rnorm(1:n)

	for (t in 2:n) {
		y1[t] <- y1[t - 1] + r1[t]	
	}

	for (t in 2:n) {
		y2[t] <- y2[t - 1] + r2[t]	
	}

	y1 <- ts(y1)
	y2 <- ts(y2)
	
	reg <- lm(y1 ~ y2)
	
	value <- coef(summary(reg))[2,3]	

	z[i] <- value
	
	if(z[i]>1.96 || z[i]< -1.96) { 
		count <- count + 1			
	}	
}

frequency <- count/runs

print(frequency)




