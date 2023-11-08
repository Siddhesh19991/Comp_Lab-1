
# a1
theta <- seq(-4, 4, 0.01)
x <- c(-2.8, 3.4, 1.2, -.3, -2.6)

y_a1 <- c()
for (theta_i in theta){
  y_a1 <- c(y_a1, -5*log(pi)-sum(log(1+(x-theta_i)**2)))
}

#plot(theta, y_a1, type="l")


# a2
y_a2 <- c()
for (theta_i in theta){
  y_a2 <- c(y_a2, sum(2*(x-theta_i)/(1+(x-theta_i)^2)))
}

#plot(theta, y_a2, type="l")
#abline(b=0, a=0)
# 3 times it crosses y=0

#a_0 <- 
#sum(2*(x-theta_i)/(1+(x-theta_i)^2))



# b and c questions: Bisection

second_derivative <- function(input){sum(2*(x-input)/(1+(x-input)^2))}

a <- -2.5
b <- -2
x_i <- c()

while(length(x_i) < 2 || abs(tail(x_i,1)-tail(x_i,2)[1])>0.00000001){
  y1 <- second_derivative(a) #sum(2*(x-a)/(1+(x-a)^2))
  y2<- second_derivative(b) #sum(2*(x-b)/(1+(x-b)^2))
  
  if(length(x_i) == 0 && y1 * y2 >= 0){print("Break");break}
  
  x_i <- c(x_i,(a + b)/2)
  
  if(second_derivative(a)*second_derivative(tail(x_i,1)) <= 0){
    b <- tail(x_i,1)
  }
  if(second_derivative(b)*second_derivative(tail(x_i,1)) < 0){
    a <- tail(x_i,1)
  }
}
cat("Estimate: ", (tail(x_i, 1)+tail(x_i, 2)[1])/2)

plot(theta, y_a1, type="l")
abline(v=c(tail(x_i,1)))

plot(theta, y_a2, type="l")
abline(b=0, a=0)
abline(v=c(tail(x_i,1)))

# c question a and b values: -0.5 and 0 (first peak, or right peak) # global
# c question a and b values: -2.5 and -2 (second peak, local peak)

