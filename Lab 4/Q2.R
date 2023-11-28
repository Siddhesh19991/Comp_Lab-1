
#a) 


w  <- 1.999
xv <- seq(-1, 1, by=0.01) * 1/sqrt(1-w^2/4) # we divide by "sqrt(1-w^2/4)" to ensure the the value below the root in the below equation is postive.
plot(xv, xv, type="n", xlab=expression(x[1]), ylab=expression(x[2]), las=1)
# ellipse
lines(xv, -(w/2)*xv-sqrt(1-(1-w^2/4)*xv^2), lwd=2, col=8)
lines(xv, -(w/2)*xv+sqrt(1-(1-w^2/4)*xv^2), lwd=2, col=8)



#b) 


# x2 given x1
# [ -(1.999/2)*x1-sqrt(1-(1-1.999^2/4)*x1^2), -(1.999/2)*x1+sqrt(1-(1-1.999^2/4)*x1^2) ] 


# x1 given x2 
# [ -(1.999/2)*x2-sqrt(1-(1-1.999^2/4)*x2^2), -(1.999/2)*x2+sqrt(1-(1-1.999^2/4)*x2^2) ]


#c) 

one<-c()
two<-c()

gibbs<- function(x_one,x_two,n){
  
  one<-c()
  two<-c()
  
  i <- 2
  
  x1<-c(x_one)
  x2<-c(x_two)
  
  a<-list()
  b<-list()
  
  while (i < (n+1)){
    
    # x1 given x2
    a[i-1]<-list(c(-(1.999/2)*tail(x2,1)-sqrt(1-(1-1.999^2/4)*tail(x2,1)^2), -(1.999/2)*tail(x2,1)+sqrt(1-(1-1.999^2/4)*tail(x2,1)^2)))
    x1[i]<-runif(1,min = a[[i-1]][1],max = a[[i-1]][2])
    
    #x2 given x1
    b[i-1]<-list(c(-(1.999/2)*tail(x1,1)-sqrt(1-(1-1.999^2/4)*tail(x1,1)^2), -(1.999/2)*tail(x1,1)+sqrt(1-(1-1.999^2/4)*tail(x1,1)^2)))
    x2[i]<-runif(1,min = b[[i-1]][1],max = b[[i-1]][2])
    
    
    i<- i+1 
  }
  one<<-x1
  two<<- x2
  #sum(one>0)/n
}


gibbs(30,-30,1000)
points(one,two)
sum(one>0)/n

gibbs(0,0,1000)
sum(one>0)/n



#The true value of the probability depends on where we put the starting values for x1 and x2. 


#d)

#This is because, the samples given when w = 1.999 is more concentrated to the top left of the ellipse while when 
# w = 1.8, the sampling values are more spread out along the ellipse. The axis limit is also larger (-30 to 30) compared to (-2 to 2)
# So it doesn't sample from the entire distribution but a portion. 


#e) 

#Do we draw the boundary for U based on the sample we got for X1 and X2 in the above part? 

#u1<- seq(8.726179,60.18003,by=0.01)

#u1<- x1-x2

u1<-seq(-63.245,63.245,by=0.01)
uv <- u1
plot(uv, uv, type="n", las=1,ylim = c(-5,5))
# ellipse
lines(uv, sqrt((4-0.001*uv^2)/3.999), lwd=2, col=8)
lines(uv, -sqrt((4-0.001*uv^2)/3.999), lwd=2, col=8)

# How do we know the values of U1 to put to get the values of U2. 


one_u<-c()
two_u<-c()

gibbs_2<- function(u_one,u_two,n){
  
  one_u<-c()
  two_u<-c()
  
  i <- 2
  
  u1<-c(u_one)
  u2<-c(u_two)
  
  a<-list()
  b<-list()
  
  while ( i < (n+1)){
    
    # u1 given u2
    a[i-1]<-list(c(-sqrt((4-3.999*tail(u2,1)^2)/0.001),sqrt((4-3.999*tail(u2,1)^2)/0.001)))
    u1[i]<-runif(1,min = a[[i-1]][1],max = a[[i-1]][2])
    
    #u2 given u1
    b[i-1]<-list(c(-sqrt((4-0.001*tail(u1,1)^2)/3.999),sqrt((4-0.001*tail(u1,1)^2)/3.999)))
    u2[i]<-runif(1,min = b[[i-1]][1],max = b[[i-1]][2])
    
    
    i<- i+1 
  }
  one_u<<- u1
  two_u<<- u2
}

gibbs_2(-20,0,1000)
points(one_u,two_u)
sum((one_u+two_u)/2>0)/1000


gibbs_2(-30,0,1000)
sum((one_u+two_u)/2>0)/1000

gibbs_2(0,0,1000)
sum((one_u+two_u)/2>0)/1000

