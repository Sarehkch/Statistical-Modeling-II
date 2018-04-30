
#Exercise 5.1

rm(list = ls())

## required library
library(mvtnorm)

#Data
data <- read.csv("G:/My Drive/PhD/Term 7 - Spring 2018/Courses/Statistical Modeling 2/Section 5/restaurants.csv", header = T)

head(data)
data$X <- NULL
head(data)

y <- scale(data[,'Profit'])

X <- scale(data[,c('DinnerService', 'SeatingCapacity')])

#Gibbs sampler

sampler<-function(Y,X,num_samples=1000,burnin=100){
  mu=matrix(c(0,0), nrow=1, ncol = 2)
  #initial values for gamma distribution
  a <-1.
  b <-1.
  d <-2
  n <- length(Y)
  k <- diag(d)
  lamb <- diag(n)
  mu_n <-matrix(NA, num_samples+burnin)
  beta_param <-matrix(NA, num_samples+burnin, ncol(X))
  omega <-matrix(NA,num_samples+burnin)
  K_n <- k+t(X)%*%lamb%*%X
  mu_n <- (t(Y)%*%lamb%*%X+(mu)%*% k) %*% (solve(K_n))
  a_n <- a + (n)/2.
  b_n <- b + 0.5*(t(Y)%*%Y + (mu)%*% k %*%t(mu) - (mu_n) %*% K_n %*% t(mu_n))
  
  for(i in 1:(num_samples+burnin)){
    omega[i]<-rgamma(n=1,shape=a_n, rate=b_n)
    beta_param[i,]<-rmvnorm(n=1,mean=mu_n,sigma=(solve(K_n)/(omega[i]*K_n)))
    
  }
  return(list("beta_param"=beta_param[(burnin+1):(burnin+num_samples),],"omega"=omega[(burnin+1):(burnin+num_samples)]))
}

Result <- sampler(y,X)

# beta parameters

FinalBeta <- as.matrix(colMeans(Result$beta_param))   


y_pred <- X %*% FinalBeta    #prediction using bayesian regression

#residuals
residuals <- y - y_pred

# plots
par(mfrow = c(1,2))

hist(data$Profit, main = 'Histogram of Profit', xlab = 'Profit', col ="blue")


hist(residuals, main = 'Histogram of residuals', col ="blue")


###################################################################

# Exercise 5.2
X <- scale(data[,'Profit'])

Sampler <- function (X,num_samples=1000){
  
  # different means
  mu_1 <- normal(0,1)
  mu_2 <- normal(1,2)
  
  # similar variance - I chose constant 
  sd <- 2
  
  n <- length(X)
  
  # latent indicator variable
  z = matrix(0, nrow =num_samples, ncol = n)
  
  # Samples
  mu_1_new <- matrix(0, num_samples)
  mu_2_new <- matrix(0, num_samples)
 
  for (i in 1:num_samples){
    
    #sample z
    # probability of X given z=1
    p1 <- dnorm(X, mu_1_new, sd)
    
    #probability of X given z=2
    p2 <- dnorm(X, mu_2_new, sd)
    
    P <- p1/(p1+p2)
    
    r = runif(1000)
    
    if (r[i] < P){
      z[i,] = 1
    }
    
else{
  z[i,] = 2
}
  }
  
  X_1 = sum(X[z==1])
  n_1 = sum(z==1)
  X_2 = sum(X[z==2])
  n_2 = sum(z==2)
 
  
   
}


