data<-read.csv2("/Users/siddheshsreedar/Downloads/prices1.csv")


### 1) 
plot(data$SqFt,data$Price)

model<-lm(Price ~ SqFt, data = data)

lines(1:3750,model$coefficients[1]+model$coefficients[2]*(1:3750), col = "red")


# Yes, a straight line does seem like a good fit for the data. 


### 2) 
#Price = b + a1*SqFt + a2*(SqFt - c)

sum(model$residuals^2)

min<-function(c,data){
  data$para<-ifelse(data$SqFt>c,data$SqFt-c,0)
  model2<-lm(Price ~ SqFt + para, data = data)
  sum(model2$residuals^2)
}


opt<-optim(2000,min,data= data,method = "Brent",lower = -500, upper = 4000)       #setting inital value as 2000 as mentioned in the assignment 

data$para<-ifelse(data$SqFt>opt$par,data$SqFt-opt$par,0)
model3<-lm(Price ~ SqFt + para, data = data)


### 3)

#110 rows 
data<-read.csv2("/Users/siddheshsreedar/Downloads/prices1.csv")


value_c<-c()
for (i in 1:10000){
  if (i %% 50 == 0) {print(sprintf("Iteration %d",i))}
  index<-sample(1:110,110,replace = TRUE)
  data2<-data[index,]
  
  min<-function(c,data){
    data$para<-ifelse(data$SqFt>c,data$SqFt-c,0)
    model2<-lm(Price ~ SqFt + para, data = data)
    sum(model2$residuals^2)
  }
  
  c_opt<-optim(2000,min,data= data2,method = "Brent",lower = -500, upper = 4000)   
  value_c<-c(value_c,c_opt$par)
}

value_c
hist(value_c)
#based on the plot, we can see 2 peaks, it looks like 2 Gaussian. 



#Bootstrap bias correction
2*opt$par-sum(value_c)/10000


#Variance
sum((value_c-mean(value_c))^2)/9999

#Confidence Interval 
library(boot)


stat<-function(data,index){
  data2<-data[index,]
  
  min<-function(c,data){
    data$para<-ifelse(data$SqFt>c,data$SqFt-c,0)
    model2<-lm(Price ~ SqFt + para, data = data)
    sum(model2$residuals^2)
  }
  
  c_opt<-optim(2000,min,data= data2,method = "Brent",lower = -500, upper = 4000)   
  c_opt$par
}
  
  
var<-boot(data = data,stat , R = 10000)
plot(var)

#the value of c from the entire data
var$t0

#the values of c from the bootstrap data
var$t


boot.ci(var)

### 4)
value_c_2<-c()
for (i in 1:110){
  if (i %% 50 == 0) {print(sprintf("Iteration %d",i))}
  data3<-data[-i,]
  
  min<-function(c,data){
    data$para<-ifelse(data$SqFt>c,data$SqFt-c,0)
    model2<-lm(Price ~ SqFt + para, data = data)
    sum(model2$residuals^2)
  }
  
  c_opt<-optim(2000,min,data= data3,method = "Brent",lower = -500, upper = 4000)   
  value_c_2<-c(value_c_2,c_opt$par)
}

value_c_2

#variance

term1<-nrow(data)*var$t0 - (nrow(data)-1)*(value_c_2)

term2<-sum(value_c_2)/nrow(data)

sum((term1-term2)^2)/(nrow(data)*(nrow(data)-1))


#Upon comparing the variance for jackknife and bootstrap, we can see that jackknife estimate has a relatively lower 
#variance as compared to the bootstrap estimate. 

#(Is it ok if lower than boostrap? cause in the PPT it says that jackknife overestimates the variance.)

### 5) 
boot.ci(var)

# We see that based on a 95% CI, the BCa gives a tighter CI while the Normal gives the broadest CI. 
# The Basic and Percentile CI, have the same difference value between the max and min range of the CI. 
# While Percentile and BCa gives a range from around 1000s to higher 3000s, Normal and Basic 
# gives a range around 2000s to mid-5000s. 

#(What kind of analysis do we do here?)
