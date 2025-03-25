#### Econometrics 1 - TD3 ####
rm(list=ls()) #Clean workspace

if(!require(car)) install.packages("car")
if(!require(estimatr)) install.packages("estimatr")

set.seed(100) #Select a random seed in order to replicate results

####################
#### Question 1 ####
####################

#Generate the data and perform regressions for every sample size
for (n in c(20,100,10^3,10^5))
{
  cat("Sample size : ", n, "\n") 
  #1. We generate one random sample
  D=rnorm(n) #Generate D from a standard normal distribution
  U=runif(n, min=-1, max=1) #Generate U from a continuous uniform distribution
  Y=1+D+(1+D)*U 
  
  #2. We estimate the model using OLS
  model=lm(Y~D)
  print(coef(summary(model)))
  cat("\n")
}


####################
#### Question 2 ####
####################

for (n in c(20,100,10^3,10^5))
{
  cat("Sample size : ", n, "\n") 
  #1. We generate a random sample
  D=rnorm(n)  # We draw D from a standard normal distribution
  U=rcauchy(n) # We draw U from a Cauchy distribution
  Y=1+D+(1+D)*U
  
  #2. We estimate the model and print results
  model=lm(Y~D)
  print(coef(summary(model)))
  cat("\n")
}

####################
#### Question 3 ####
####################

# We define a function that performs the simulation for a given sample size n
My_Simul <- function(n)
{
  D=rnorm(n)
  U=runif(n, min=-1, max=1)
  Y=1+D+(1+D)*U
  model=lm(Y~D)
  
  #Coefficients associated with D
  beta_2=coef(model)["D"] 
  
  return(beta_2)
}

#We run this function for n=20, 1000 times
simul_20=replicate(1000, My_Simul(20))
simul_20[c(1:5)]

#We run this function for n=1000, 1000 times
simul_1000=replicate(1000, My_Simul(1000))

#We plot the densities
Title_beta2=quote(paste("Density of ", widehat(beta)[2]))
par(xpd=F)
#n=20
plot(density(simul_20), main=substitute(title*N, list(title=Title_beta2, N=" (n = 20)")),ylim=c(0,max(density(simul_20)$y)+0.1) ,lwd=2)
curve(dnorm(x, mean=mean(simul_20), sd=sd(simul_20)), col="blue", add=TRUE, lwd=2) #Comparison with normal distribution
legend(x="topright", legend=c("Density","Normal"), col=c("black","blue"), pch=16)

#n=1000
plot(density(simul_1000), main=substitute(title*N, list(title=Title_beta2, N=" (n = 1000)")), lwd=2,col="red")
curve(dnorm(x, mean=mean(simul_1000), sd=sd(simul_1000)), col="blue", add=TRUE, lwd=2) #Comparison with normal distribution
legend(x="topright", legend=c("Density","Normal"), col=c("red","blue"), pch=16)

#Facilitate comparison by putting both graphs together
plot(density(simul_20), main=Title_beta2, ylim=c(0,max(density(simul_1000)$y)+0.5), lwd=2)
lines(density(simul_1000), col="red", lwd=2)
legend(x="topright", legend=c("n=20","n=1000"), col=c("black","red"), pch=16)

#BONUS: Comparison to a standard normal (The asymptotic variance is equal to 4/3)
simul_20_sd<-(simul_20-1)*sqrt(20/(4/3))
simul_1000_sd<-(simul_1000-1)*sqrt(1000/(4/3))
plot(density(simul_20_sd), main=Title_beta2, ylim=c(0,max(density(simul_1000_sd)$y)+0.5), lwd=2)
lines(density(simul_1000_sd), col="red", lwd=2)
curve(dnorm(x, mean=0, sd=1), col="blue", add=TRUE, lwd=2) # 
legend(x="topright", legend=c("n=20","n=1000","Standard Normal"), col=c("black","red","blue"), pch=16)

####################
#### Question 4 ####
####################

#We define the function where we can choose n, var_D
My_Simul_2 <- function(n=1000, var_D=1, a=1)
{
  D=rnorm(n, 0, sqrt(var_D))
  U=runif(n, min=-a, max=a)
  Y=1+D+U
  model=lm(Y~D)
  beta_2=coef(model)["D"]
  return(beta_2)
}

#1) We consider Var_D=1 and var_D=4
res_var1 =replicate(1000,My_Simul_2(var_D=1))
res_var4 =replicate(1000,My_Simul_2(var_D=4))

#We plot the densities
plot(density(res_var1),xlim=c(0.93,1.07), ylim=c(0,90), lwd=2,
     main=Title_beta2)
lines(density(res_var4), col="red", lwd=2)
legend(x="topright", legend=c("D ~ N(0,1)","D ~ N(0,4)"), col=c("black","red"), pch=16)

#2)We consider a=1 and a=2
res_a1 = replicate(1000,My_Simul_2(a=1))
res_a2 = replicate(1000,My_Simul_2(a=2))

  
plot(density(res_a1),xlim=c(0.85,1.15), ylim=c(0,23), main=Title_beta2, lwd=2)
lines(density(res_a2), col="red", lwd=2)
legend(x="topright", legend=c("a = 1","a = 2"), col=c("black","red"), pch=16)

cat("Variance of estimator with var(D)=1 et a=1 :"  , var(res_var1),
    "\nVariance of estimator with c var(D)=4        :", var(res_var4),
    "\nVariance of estimator with  a=2             :", var(res_a2))


####################
#### Question 5 ####
####################

#We use the package "car" for tests and "estimatr" for robust standard errors
library(estimatr)
library(car)

help(lm_robust) #Function allowing to compute robust standard errors
help(lht) #!function for linear tests

#Function to obtain pvalues
My_Simul_p_val <- function(n)
{
  D=rnorm(n)
  U=runif(n, min=-1, max=1)
  Y=1+D+(1+D)*U

  #We estimate the model under homoskedasticity assumption and with robust standard errors
  model_homo=lm(Y~D)
  model_hetero=lm_robust(Y~D, se_type = "stata")
  
  #p-value of test with non-robust s.e.
  p_homo = lht(model_homo, "D = 1", test="F")[2,"Pr(>F)"] 
  
  #p-value of test with robust s.e.
  p_hetero = lht(model_hetero, "D = 1", test="F")[2,"Pr(>F)"] 
  
  return(c(p_homo=p_homo,p_hetero=p_hetero))
}
#Simulations
simul_20   = replicate(1000,My_Simul_p_val(n=20))
simul_1000 = replicate(1000,My_Simul_p_val(n=1000))

print(simul_20[,c(1:5)])

#We test the percentage of times we reject the test
#with n=20
rejet_1pct_homo_20 =sum(simul_20[1,]<0.01)/1000
rejet_5pct_homo_20 =sum(simul_20[1,]<0.05)/1000
rejet_10pct_homo_20=sum(simul_20[1,]<0.10)/1000
rejet_homo_20=c(rejet_1pct_homo_20,rejet_5pct_homo_20,rejet_10pct_homo_20)

rejet_1pct_hetero_20 =sum(simul_20[2,]<0.01)/1000
rejet_5pct_hetero_20 =sum(simul_20[2,]<0.05)/1000
rejet_10pct_hetero_20=sum(simul_20[2,]<0.10)/1000
rejet_hetero_20=c(rejet_1pct_hetero_20,rejet_5pct_hetero_20,rejet_10pct_hetero_20)

#With n=1000
rejet_1pct_homo_1000 =sum(simul_1000[1,]<0.01)/1000
rejet_5pct_homo_1000 =sum(simul_1000[1,]<0.05)/1000
rejet_10pct_homo_1000=sum(simul_1000[1,]<0.10)/1000
rejet_homo_1000=c(rejet_1pct_homo_1000,rejet_5pct_homo_1000,rejet_10pct_homo_1000)

rejet_1pct_hetero_1000 =sum(simul_1000[2,]<0.01)/1000
rejet_5pct_hetero_1000 =sum(simul_1000[2,]<0.05)/1000
rejet_10pct_hetero_1000=sum(simul_1000[2,]<0.10)/1000
rejet_hetero_1000=c(rejet_1pct_hetero_1000,rejet_5pct_hetero_1000,rejet_10pct_hetero_1000)

#We show the results

res_n_20=data.frame(N=rep(20,6),
                    Type_var=rep(c("Homo","Hetero"), each=3),
                    Level=rep(c("1%","5%","10%"),2),
                    pct_rejet=c(rejet_homo_20,rejet_hetero_20))

res_n_1000=data.frame(N=rep(1000,6),
                      Type_var=rep(c("Homo","Hetero"), each=3),
                      Level=rep(c("1%","5%","10%"),2),
                      pct_rejet=c(rejet_homo_1000,rejet_hetero_1000))

print(res_n_20)
print(res_n_1000)


####################
#### Question 6 ####
####################
#We modify the test
My_Simul_p_val2 <- function(n)
{
  D=rnorm(n)
  U=runif(n, min=-1, max=1)
  Y=1+D+(1+D)*U
  
  #We estimate the model under homoskedasticity assumption and with robust standard errors
  model_homo=lm(Y~D)
  model_hetero=lm_robust(Y~D, se_type = "stata")
  
  #p-value of test with non-robust s.e.
  p_homo = lht(model_homo, c("D = 1","(Intercept)=1"), test="F")[2,"Pr(>F)"] 
  
  #p-value of test with robust s.e.
  p_hetero = lht(model_hetero,c("D = 1","(Intercept)=1"), test="F")[2,"Pr(>F)"] 
  
  return(c(p_homo=p_homo,p_hetero=p_hetero))
}
#Simulations
simul_20   = replicate(1000,My_Simul_p_val2(n=20))
simul_1000 = replicate(1000,My_Simul_p_val2(n=1000))

print(simul_20[,c(1:5)])

#We test the percentage of times we reject the test
#with n=20
rejet_1pct_homo_20 =sum(simul_20[1,]<0.01)/1000
rejet_5pct_homo_20 =sum(simul_20[1,]<0.05)/1000
rejet_10pct_homo_20=sum(simul_20[1,]<0.10)/1000
rejet_homo_20=c(rejet_1pct_homo_20,rejet_5pct_homo_20,rejet_10pct_homo_20)

rejet_1pct_hetero_20 =sum(simul_20[2,]<0.01)/1000
rejet_5pct_hetero_20 =sum(simul_20[2,]<0.05)/1000
rejet_10pct_hetero_20=sum(simul_20[2,]<0.10)/1000
rejet_hetero_20=c(rejet_1pct_hetero_20,rejet_5pct_hetero_20,rejet_10pct_hetero_20)

#With n=1000
rejet_1pct_homo_1000 =sum(simul_1000[1,]<0.01)/1000
rejet_5pct_homo_1000 =sum(simul_1000[1,]<0.05)/1000
rejet_10pct_homo_1000=sum(simul_1000[1,]<0.10)/1000
rejet_homo_1000=c(rejet_1pct_homo_1000,rejet_5pct_homo_1000,rejet_10pct_homo_1000)

rejet_1pct_hetero_1000 =sum(simul_1000[2,]<0.01)/1000
rejet_5pct_hetero_1000 =sum(simul_1000[2,]<0.05)/1000
rejet_10pct_hetero_1000=sum(simul_1000[2,]<0.10)/1000
rejet_hetero_1000=c(rejet_1pct_hetero_1000,rejet_5pct_hetero_1000,rejet_10pct_hetero_1000)

#We show the results

res_n_20=data.frame(N=rep(20,6),
                    Type_var=rep(c("Homo","Hetero"), each=3),
                    Level=rep(c("1%","5%","10%"),2),
                    pct_rejet=c(rejet_homo_20,rejet_hetero_20))

res_n_1000=data.frame(N=rep(1000,6),
                      Type_var=rep(c("Homo","Hetero"), each=3),
                      Level=rep(c("1%","5%","10%"),2),
                      pct_rejet=c(rejet_homo_1000,rejet_hetero_1000))

print(res_n_20)
print(res_n_1000)




