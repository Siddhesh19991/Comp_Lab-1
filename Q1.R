

theta <- seq(-4, 4, 0.1)


x <- c(-2.8, 3.4, 1.2, -.3, -2.6)

y <- c()
for (theta_i in theta){
  y <- c(y, -5*log(pi)-sum(log(1+(x-theta)**2)))
}
