
rm(list = ls())

#data - only first 10 data points of faithful data 

data = read.csv("G:/My Drive/PhD/Term 7 - Spring 2018/Courses/Statistical Modeling 2/Section 4/faithful.csv", header = TRUE)
x <- as.vector(data$waiting[1:10]) 
y <- scale(data$eruptions[1:10])
N <- length(x)

set.seed(111)
#squared exponential function
k.matrix <- function(x, alpha, l, sigma) {
  
  
  k1 <- matrix(rep(0, length(x)*length(x)), nrow = length(x))
  
  for (i in 1:nrow(k1)){
    for (j in 1:ncol(k1)){
      
      k1[i,j] <- alpha^2 * exp(-0.5 / (l^2) * (abs(x[i] - x[j]))^2)
    }
  }
  
  k <- k1 + sigma^2 * diag(N)
  
  return(k)
}

#negative log-likelihood
negative.log.likelihood<- function(theta, x, y) {
  
  k <- k.matrix(x, alpha = theta[1], l = theta[2], sigma = theta[3])
  K.inv <- solve(k)
  
  nlogl <- as.numeric(0.5*t(y) %*% K.inv %*% y + 0.5*determinant(k,logarithm=T)$modulus) + 0.5*N*log(2*pi)
  return(nlogl)
}

# Optimization

optimization <- optim(par = c(0.5, 0.01, 0.05), fn = negative.log.likelihood, gr = NULL, method ="L-BFGS-B", x, y)
hyparam = optimization$par

## plotting

K.matrix <- function (X1, X2, alpha, l, sigma) {
  
  k1 <- matrix(rep(0, length(X1)*length(X2)), nrow = length(X1))
  
  for (i in 1:nrow(k1)){
    for (j in 1:ncol(k1)) {
      k1[i,j] <- (alpha^2) * exp(-0.5*(abs(X1[i] - X2[j])/l)^2)
    }
  }
  
  K <- k1 + sigma^2 * diag(nrow(k1))
  
  return(K)
}

x.star <- seq(min(x)-20, max(x)+20, length.out = 10)

#mean and cov of fuction with optimizing values

A <- K.matrix (x, x, hyparam[1], hyparam[2], hyparam[3]) + 1* diag(1,length(x))
meanfstar <- K.matrix (x.star, x, hyparam[1], hyparam[2], hyparam[3]) %*% solve(A) %*% y
covfstar = K.matrix (x.star, x.star, hyparam[1], hyparam[2], hyparam[3]) - (K.matrix (x.star, x,  hyparam[1], hyparam[2], hyparam[3] ) %*% solve(A) %*% K.matrix (x, x.star,  hyparam[1], hyparam[2], hyparam[3]))

# 95% credible interval
lower = meanfstar - 1.96 * sqrt(diag(covfstar)) 
upper = meanfstar + 1.96 * sqrt(diag(covfstar)) 

data1 = data.frame(cbind(x,y))

ggplot() +
  geom_point(data = data1, aes(x,y), colour = 'red', size =3) +
  labs(x = "waiting", y="eruptions")+
  geom_line(aes(x.star, meanfstar))+
  geom_ribbon(aes(x.star, ymin = lower, ymax = upper), alpha = 0.5 )
  