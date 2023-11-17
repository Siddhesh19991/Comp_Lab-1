custom_density <- function(x) {
  
  ifelse(x < -1 | x > 1, 0,
         ifelse(-1 <= x & x <= 0, x + 1,
                ifelse(0 < x & x <= 1, 1 - x, 0))) 
}

# Generate x values
x <- seq(-2, 2,length.out = 1000)

# Calculate the custom density values
y <- custom_density(x)

# Plot the custom density function
plot(x, y, type = "l", col = "blue", xlab = "Value", ylab = "Density",ylim = c(0,2))


#Uniform distribution as envelop 

uniform_density <- function(x){
  ifelse(x < -1 | x > 1, 0,
         ifelse(-1 <= x & x <= 0, 1,
                ifelse(0 < x & x <= 1, 1, 0))) 
}

y1<- uniform_density(x)


#Not required to scale since scaling would lead to more wastage. 

lines(x, y1, type = "l", lty = 1, col = "red")


#a)

#rejection_sampling 

sampling<- function(amount){
  values<-c()
  
  i<-1
  while(length(values)<amount){
    U<-runif(1)
    
    Y<-runif(1,min = -1,max =1)
    
    if(U <= custom_density(Y)/uniform_density(Y)){
      values[i]<-Y
      i=i+1
    }
  }
  hist(values)
  return(values)
}

sampling(100)


#b) 

x_pos <- function(u){
  a<-1-sqrt(1-u)
  #hist(a)
  return(a)
}

x_neg <- function(u){
  a<-sqrt(1-u)-1
  #hist(a)
  return(a)
}

mixing_para_positive<-0.5
mixing_para_negative<-0.5

mixture2<- function(n,m_p,m_n){
  u <- runif(1000)
  a<-sample(c(1,2),n,replace = TRUE,prob = c(m_p,m_n))
  
  final<-ifelse(a==1,x_pos(u),x_neg(u))
  
  hist(final,breaks=100)
  return(final)
}

mixture2(10000,mixing_para_positive,mixing_para_negative)


#################
y1 <- function(x){
  ifelse(x < 0, 0,
         ifelse(0 <= x & x <= 1, 1-sqrt(1-x),0))
}

positive_Y<-y1(x)
plot(x, positive_Y, type = "l", col = "blue", xlab = "Value", ylab = "Density")

y2<- function(x){
  ifelse(x < -1, 0,
         ifelse(0 >= x & x >= - 1, -1+sqrt(1-x),0))
}

negative_Y<-y2(x)
plot(x, negative_Y, type = "l", col = "blue", xlab = "Value", ylab = "Density")     #is this how -Y should look like? 

#composition sampling

mixing_para_positive<-0.6
mixing_para_negative<-0.4

mixture<-function(n,m_p,m_n,y1,y2){
  
  y<-runif(n,min = -1, max = 1)
  
  a<-sample(c(1,2),n,replace = TRUE,prob = c(m_p,m_n))
  
  final<-ifelse(a==1,y1(y),y2(y))        
  
  hist(final)                          #Not getting any negative values
  return(final)
}

mixture(100,mixing_para_positive,mixing_para_negative,y1,y2)
#######################


#c) 

diff<- function(n){
  U1<-runif(n)
  U2<-runif(n)
  
  a<-U1-U2 
  
  hist(a)
  return(a)
}

diff(10000)



#d) 
a<-sampling(10000)
b<-mixture2(10000,mixing_para_positive,mixing_para_negative)
c<-diff(10000)

#Based on the 3, I would prefer to generate the samples using c) as it was the most straightforward way to implement. 

var(c)



