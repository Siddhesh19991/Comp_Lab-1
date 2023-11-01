
# a1
theta <- seq(-4, 4, 0.1)
x <- c(-2.8, 3.4, 1.2, -.3, -2.6)

y_a1 <- c()
for (theta_i in theta){
  y_a1 <- c(y_a1, -5*log(pi)-sum(log(1+(x-theta_i)**2)))
}

plot(theta, y_a1, type="l")


# a2
y_a2 <- c()
for (theta_i in theta){
  y_a2 <- c(y_a2, sum(2*(x-theta_i)/(1+(x-theta_i)^2)))
}

plot(theta, y_a2, type="l")
abline(b=0, a=0)
# 3 times it crosses y=0

a_0 <- 
sum(2*(x-theta_i)/(1+(x-theta_i)^2))



# b: Bisection
  


