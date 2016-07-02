# Maximum-likelihood-estimation-Regression-models

#using bblm  package to obtain maximum likelihood estimates
install.packages("bbmle")
library(bbmle)

#Maximum likelihood estimation - linear regression
#linear regression between Corn yield and Corn nitrate 
reg1 = lm(Corn$yield~Corn$nitrate)

#add the predicted value through linear regression
Corn = cbind(Corn,(6148.50+51.52*Corn$nitrate))


# Save the estimated yield values in new column "Linear"

colnames(Corn)[3]= "Linear"

View(Corn)

#function to calculate log likelihood that a value observed in a population

LLreg = function(A,B,sig1)
{
  x = Corn$yield-A*(1-exp(1-B*Corn$nitrate))
  LLsum = sum(dnorm(x,mean = 0,sd = sig1,log = T))
  return(-1*LLsum)
}

# Run the MLE2 function by giving list of start values of the parameters 
#For maximum likelihood, this function returns the parameter estimates

# Refer document for explaination of  start values

res = mle2(minuslogl = LLreg,start = list(A =-mean(Corn$yield)/1.713,B=0,sig1=1000)) 

summary(res)

# Save the estimated yield values in new column "Growth"

growth = - 4183.0*(1 - exp(1+0.00209*Corn$nitrate))

Corn = cbind(Corn,growth)

colnames(Corn)[4]= "Growth"

View(Corn)



#use nummetrics to compare the two models 
  
nummetrics=function(a,m)
{
  metrics=c(MAD=0,MSE=0,MAPE=0,MPSE=0,MADtrim=0)
  metrics["MAD"]=mean(abs(a-m))
  metrics["MSE"]=mean((a-m)^2)
  metrics["MAPE"]=mean(abs((a-m)/a))
  metrics["MPSE"]=mean(((a-m)/a)^2)
  SST=sum((a-mean(a))^2)
  SSE=sum((a-m)^2)
  metrics["R2"]=1-(SSE/SST)
  metrics["MADtrim"]=mean(abs(a-m),trim=0.1)
  return(metrics)
}

#Calculate MAPE,R2 and other accuracy metrics for linear regression model

a = Corn$yield
m = 6148.50+51.52*Corn$nitrate

nummetrics(a,m)

#Calculate MAPE,R2 and other accuracy metrics for Von Bertalanffy growth model

a = Corn$yield
m = - 4183.0*(1 - exp(1+0.00209*Corn$nitrate))



#2.Maximum likelihood estimation - Zero Inflated poissson distribution
#Read the file(Health)

Health <- read.csv("E:/D/UCONN-2016/spring_2016/DataMiningwithR/Assignments-2/Health.csv")

View(Health)

#function to calculate log likelihood that a value observed in a population

LLregbin = function(a0,a1,a2,b0,b1,b2,b3)
{

  x1=a0+a1*(Health$numchron)+a2*(Health$male)
    
  # probability of being healthy - "P"
  p=exp(x1)/(1+exp(x1))
  x2 = b0+(b1*Health$numchron)+(b2*Health$employed)+(b3*Health$married)
  
  #POison distribution - "l" mean of healthy visits  
    l = exp(x2)
    
    L = ifelse(Health$ofp==0, p+((1-p)*(dpois(0,lambda=l))), (1-p)*dpois(Health$ofp,lambda = l)) 
  # sum of log likelihood values  
    LLsum = sum(log(L)) 
    
    return(-1*LLsum)

  }

# Run the MLE2 function by giving list of start values of the parameters 
# For maximum likelihood, this function returns the parameter estimates

# Refer document for explaination on  start values

res2 = mle2(minuslogl = LLregbin,start = list(a0=0,a1=0,a2=0,b0=1,b1=0,b2=0,b3=0))
summary(res2)


#Parameter estimates

# x1= -1.197 - 0.542 *(Health$numchron) + 0.373 *(Health$male)

# probability of being healthy - p = exp(x1)/(1+ exp(x1)

# x2 = 1.677 + (0.149 *Health$numchron)+(0.049 *Health$employed)-0.0576*Health$married

#POison distribution - "l" mean of healthy visits  
# l = exp(x2)



# 3.Maximum likelihood estimation - Non linear regression


spending <- read.csv("E:/D/UCONN-2016/spring_2016/DataMiningwithR/Assignments-2/spending.csv")

View(spending)

#function to calculate log likelihood that a value observed in a population


LLspend = function (a0, b1, t, sig1, sig2 )
{  
  # first regression equation
  a = spending$amount - a0
  # second regression equation
  b =spending$amount-a0-b1*(spending$age-t)
  
  LLSum = sum(ifelse(spending$age > t,dnorm(b, mean = 0, sd = sig2, log = T ) , dnorm(a, mean = 0, sd = sig1,log = T ) ))  
  return(-1*LLSum)

}


# Run the MLE2 function by giving list of start values of the parameters 
# For maximum likelihood, this function returns the parameter estimates

# Refer document for explaination on  start values


res3 = mle2(minuslogl = LLspend,start = list(a0= mean(spending$amount),b1=0,t = mean(spending$age), sig1= 1000,sig2=1000))

summary(res3)
