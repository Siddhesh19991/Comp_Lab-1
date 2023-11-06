a<-c(1,2,3,4)
#a)
myvar <- function(x){
  
  len<-length(x)
  a<- 1/(len-1)
  b<-sum(x^2)
  c<-(sum(x))^2
  
  final<-a*(b-(1/len)*(c))
  
  return(final)
}
####
myvar2 <- function(x){
  output <- (sum(x^2) - sum(x)^2 / length(x)) / (length(x)-1)
  
  #cat("Variance estimate:", output, "\n")
  return(output)
}

myvar_vector <- c()
var_vector <- c()
for (i in 1:10^4){
  myvar_vector <- c(myvar_vector, myvar(x[1:i]))
  var_vector <- c(var_vector, var(x[1:i]))
}
difference <- myvar_vector - var_vector

plot(difference, type="p")
plot(myvar_vector, var_vector)
###

myvar(a)

#b)
mean_value <- 10^8
sd <- 1

set.seed(78123)
x <- rnorm(10000, mean = mean_value, sd = sd)

#c) 

subset<-list()
for (i in 1:10000){
  subset[[i]]<-x[1:i]
}

y<-list()
for (i in 1:10000){
  y[[i]]<-myvar(subset[[i]])-var(subset[[i]])
}

y<-unlist(y)
i<-1:10000
plot(i,y)


#as the subset of numbers in the vector increases the difference between the myvar and var function increases. 


#d) 

myvar2 <- function(x){
  
  len<-length(x)
  a<- 1/(len-1)
  c<-sum(x-mean(x))^2
  
  final<-a*c
  
  return(final)
}

myvar2(a)

y<-list()
for (i in 1:10000){
  y[[i]]<-myvar2(subset[[i]])-var(subset[[i]])
}

y<-unlist(y)
i<-1:10000
plot(i,y)


myvar3 <- function(x){
  
  len<-length(x)
  a<- 1/(len)
  b<-sum(x^2)
  c<-(sum(x))^2
  
  final<-a*(b-(1/len)*(c))
  
  return(final)
}

y<-list()
for (i in 1:10000){
  y[[i]]<-myvar3(subset[[i]])-var(subset[[i]])
}

y<-unlist(y)
i<-1:10000
plot(i,y)
