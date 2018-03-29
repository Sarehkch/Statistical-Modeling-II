
##Exercise 2.12

## required library
library(mvtnorm)

##gibbs sampler

sampler<-function(Y,X,num_samples=1000,burnin=100){
  mu=matrix(c(0,0), nrow=1, ncol = 2)
  a=1.
  b=1.
  p =2
  n = length(Y)
  k= diag(p)
  lamb = diag(n)
  mu_n<-matrix(NA, num_samples+burnin)
  beta_param<-matrix(NA, num_samples+burnin, ncol(X))
  omega<-matrix(NA,num_samples+burnin)
  K_n = k+t(X)%*%lamb%*%X
  mu_n <- (t(Y)%*%lamb%*%X+(mu)%*% k) %*% (solve(K_n))
  a_n <- a + (n)/2.
  b_n <- b + 0.5*(t(Y)%*%Y + (mu)%*% k %*%t(mu) - (mu_n) %*% K_n %*% t(mu_n))
  
  for(i in 1:(num_samples+burnin)){
    omega[i]<-rgamma(n=1,shape=a_n, rate=b_n)
    beta_param[i,]<-rmvnorm(n=1,mean=mu_n,sigma=(solve(K_n)/(omega[i]*K_n)))
                            
  }
  return(list("beta_param"=beta_param[(burnin+1):(burnin+num_samples),],"omega"=omega[(burnin+1):(burnin+num_samples)]))
}

##importing data

data = read.csv("C:/Users/Sareh/Google Drive/PhD/Term 7/Courses/Statistical Modeling 2/Section 2/dental.csv", header = TRUE)


## adding an intercept

data = cbind(data,1)

## splitting data
data1 = data[,-4]    # removing subject
data1 = data1[,-4]   # removing sex
Y = data1[,2]
Y = data.matrix(Y)   #converting numeric to matrix
X = data1[,3:4]
X = data.matrix(X)   #converting numeric to matrix

result1 = sampler(Y,X)
FinalBeta1 = as.matrix(colMeans(result1$beta_param))   
FinalOmega1 = mean(result1$omega)

Y_pred_1 = X %*% FinalBeta1    #prediction using bayesian regression

##
plot(X[,1],Y,xlab="age", ylab="dental distance")
abline(FinalBeta1[2],FinalBeta1[1],col="red")
lm_model = lm(distance ~ age, data = data)      #frequentist regression
abline(lm_model, col="blue")

plot(Y~X[,1])
for (i in 1:1000){abline(result1$beta_param[i,2],result1$beta_param[i,1])}


# ridge regression
#install.packages("glmnet")
library(glmnet)
ridge_model = cv.glmnet(X, Y, alpha = 0)

############################################################################################

##Exercise 2.14

## required library
library(mvtnorm)
library(MASS)

##gibbs sampler

sampler<-function(Y,X,num_samples=1000,burnin=100){
  mu0=matrix(c(0,0), nrow=1, ncol = 2)
  a=1.
  b=1.
  n = length(Y)
  p =2
  k= diag(p)
  mu<-matrix(NA, num_samples+burnin)
  beta_param<-matrix(NA, num_samples+burnin, ncol(X))
  omega<-matrix(NA,num_samples+burnin)
  omega[,1] = a/b
  lambda<-matrix(NA,nrow(X), num_samples+burnin)
  lambda[,1] = 1.0
  beta_param[,1] = 0
  tau = 1
  
  for(i in 2:(num_samples+burnin)){
    
    lamb = diag(lambda[,i-1])
    K_n = k +(t(X)%*% lamb %*% X)
    mu_n <- (t(Y) %*% lamb %*% X +(mu)%*% k) %*% (solve(K_n))
    a_n <- a + (n/2).
    b_n <- beta0 + 0.5*(t(Y)%*% lamb %*%Y +(mu)%*% k %*%t(mu) - (mu_n) %*% K_n %*% t(mu_n))
    
    omega[i]<-rgamma(n=1,shape=a_n, rate=b_n)
    beta_param[i,]<-rmvnorm(n=1,mean=mu_n,sigma=((solve(K_n))/(omega[i-1])))
    
    for (n in 1:nrow(X)){
      lambda[n,i] <- rgamma(n=n, tau+0.5, tau + 0.5*(omega[i] * (Y - X %*% beta_param[i,])**2))
    }
    
  }
  return(list("beta_param"=beta_param[(burnin+1):(burnin+num_samples),],"omega"=omega[(burnin+1):(burnin+num_samples)], "lamb"=lamb[(burnin+1):(burnin+num_samples)]))
}

##importing data

data = read.csv("C:/Users/Sareh/Google Drive/PhD/Term 7/Courses/Statistical Modeling 2/Section 2/dental.csv", header = TRUE)


## adding an intercept

data = cbind(data,1)

## splitting data
data1 = data[,-4]    # removing subject
data1 = data1[,-4]   # removing sex
Y = data1[,2]
Y = data.matrix(Y)   #converting numeric to matrix
X = data1[,3:4]
X = data.matrix(X)   #converting numeric to matrix

result2 = sampler(Y,X)
FinalBeta2 = as.matrix(colMeans(result2$beta_param))   
FinalOmega2 = mean(result2$omega)

##plotting
plot(X[,1],Y,xlab="age", ylab="dental distance")
abline(FinalBeta1[2],FinalBeta1[1],col="red")
lm_model = lm(distance ~ age, data = data)      #frequentist regression
abline(lm_model, col="blue")
abline(FinalBeta2[2],FinalBeta2[1],col="green")

