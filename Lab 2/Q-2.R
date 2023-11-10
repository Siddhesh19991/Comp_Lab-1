
x <- c(0,0,0,0.1,0.1,0.3,0.3,0.9,0.9,0.9)
y<- c(0,0,1,0,1,1,1,0,1,1)


dose <- function(beta0,beta1,third){
  
  x <- c(0,0,0,0.1,0.1,0.3,0.3,0.9,0.9,0.9)
  y<- c(0,0,1,0,1,1,1,0,1,1)
  
  value<-list()
  
  b0<-beta0
  b1<-beta1
  alpha <- third
  orginal_x <- as.matrix(c(1,0))
  grad_count<-0
  main_count<-0
  
  derivative<- function(b0,b1){
    for (i in 1:length(x)){
      value[[i]]<-(y[i] - 1/(1+exp(-b0-b1*x[i])))*as.matrix(c(1,x[i]))
    }
    
    abc<-as.matrix(c(0,0))
    for (i in 1:length(x)){
      abc <- abc+ value[[i]]
    }
    grad_count<<-grad_count +1
    return(abc)
  }
  
  
  next_x<- function(b0,b1){
    as.matrix(c(b0,b1)) + alpha * derivative(b0,b1)
  }
  
  
  main_function<- function(b0,b1){
    comp<-c()
    for (i in 1:length(x)){
      comp[i]<-y[i]*log(solve(1+exp(-b0-b1*x[i]))) + (1-y[i])*log(1-solve((1+ exp(-b0-b1*x[i]))))
    }
    comp<-sum(comp)
    
    main_count<<- main_count+1
    return(comp)
  }
  
  i<-1
  
  store<-list(as.vector(orginal_x))
  
  #####
  i <- i + 1
  a<-next_x(orginal_x[1],orginal_x[2]) 
  while(main_function(a[1],a[2])<main_function(orginal_x[1],orginal_x[2])){
    alpha<-alpha/2
    a<-next_x(orginal_x[1],orginal_x[2])
  }
  
  orginal_x<-a
  store[[i]]<-as.vector(orginal_x)
  alpha<-1
  
  #####
  while(any(abs(store[[length(store)]]-store[[length(store)-1]])>0.00001)){
    
    alpha<-1
    i <- i + 1
    
    a<-next_x(orginal_x[1],orginal_x[2])
    
    while(main_function(a[1],a[2])<main_function(orginal_x[1],orginal_x[2])){
      alpha<-alpha/2
      
      a<-next_x(orginal_x[1],orginal_x[2])
    }
    
    
    orginal_x<-a
    store[[i]]<-as.vector(orginal_x)
  }
  
  #the values of b0 and b1 + the number of time the gradient and log (main) function has been called
  list("values" = store[[length(store)]],"gradient function count" =  grad_count,"likelihood function count" = main_count) 
}



# b) 
dose(-0.2,1,1)

# From the above the  function, the values are:
#$values
#[1] -0.009354408  1.262803143
#$`gradient function count`
#[1] 66
#$`likelihood function count`
#[1] 132


#comparing with a variant:
dose(-0.2,1,5)

# From the above the  function, the values are:
#$values
#[1] -0.009352896  1.262807178
#$`gradient function count`
#[1] 70
#$`likelihood function count`
#[1] 140

#c)

opt<- function(a){
  comp<-c()
  for (i in 1:10){
    comp[i]<-y[i]*log(solve(1+exp(-a[1]-a[2]*x[i]))) + (1-y[i])*log(1-solve((1+ exp(-a[1]-a[2]*x[i]))))
  }
  comp<- - sum(comp)
}

#Using BFGS
optim(c(-0.2,1),fn = opt , method = "BFGS")
#$par
#[1] -0.009356112  1.262812883
#$counts
#function gradient 
#12        8 

#Using Nelder-Mead
optim(c(-0.2,1),fn = opt , method = "Nelder-Mead", hessian = FALSE)
#$par
#[1] -0.009423433  1.262738266
#$counts
#function gradient 
#47       NA 


#Yes, the results for both is almost the same with some changes in the last few decimal places and both are almost same
# to the value found in option b) 
#The counts for both is shown above^



#d) 
glm(y~x,family = "binomial")

#from the above function, the below are the values which is roughly the same. 
#(Intercept)            x  
#-0.00936      1.26282  
