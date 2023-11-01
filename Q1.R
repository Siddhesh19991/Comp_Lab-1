
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
  
a <- -4
b <- 4 

x_i <- c()

while(length(x_i) < 2 || abs(tail(x_i,1)-tail(x_i,2)[1])>0.000001){
  
#for(i in 1:1000){   
  y1<- sum(2*(a-theta_i)/(1+(a-theta_i)^2))
  y2<- sum(2*(b-theta_i)/(1+(b-theta_i)^2))
  
  x_i <- c(x_i,(a + b)/2) 
  
  print((a+b)/2)
  
  if(y1*y2 <= 0){
    b <- tail(x_i,1)
  }else {
    a <- tail(x_i,1)
  }
  
}

tail(x_i,1)
abline(v=c(tail(x_i,1)))


