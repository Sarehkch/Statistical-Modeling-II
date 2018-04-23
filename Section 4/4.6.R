
#Exercise 4.6

## optimizing using gradient functions

# required library
library(matrixcalc)
library(MASS)

#squared exponential covariance function,

k.matrix <- function(x, alpha , l , sigma ) {
  
  kernel <- matrix(rep(0, length(x)*length(x)), nrow = length(x))
  
  for (i in 1:nrow(kernel)){
    for (j in 1:ncol(kernel)){
      
      kernel[i,j] <- alpha^2 * exp(-0.5 / (l^2) * (abs(x[i] - x[j]))^2)
    }
  }
  
  K.y <- kernel + sigma^2 * diag(nrow(kernel))
  
  return(K.y)
}


# gradient function w.r.t alpha
ga <- function (x, Y, alpha, l, sigma){
  
  a <- solve(k.matrix(x, alpha, l, sigma)) %*% Y
  M <- matrix(rep(0,length(x)*length(x)), nrow = length(x))
  for (i in 1:nrow(M)){
    for (j in 1:ncol(M)) {
      M[i,j] <- 2 * alpha * exp(-0.5 / (l^2) * (abs(x[i] - x[j]))^2)
    }
  }
  
  ga <- 0.5 * matrix.trace(a%*%t(a) - solve(k.matrix(x=x, alpha = alpha, l=l, sigma = sigma))%*%M)
  return (ga) 
}



# gradient function w.r.t length scale (l)  
gl<- function (x, Y, alpha, l, sigma){
  
  L <- matrix(rep(0,length(x)*length(x)), nrow = length(x))
  for (i in 1:nrow(L)){
    for (j in 1:ncol(L)) {
      L[i,j] <- ((alpha^2)*((x[i]-x[j])^2)*exp(-0.5 / (l^2) * (abs(x[i] - x[j]))^2))/(l^3)
    }
  }
  
  a = solve(k.matrix(x, alpha, l, sigma)) %*% Y
  gl <- 0.5 * matrix.trace(a%*%t(a) - solve(k.matrix(x, alpha, l, sigma))%*% L)
  return (gl)  
  
}

# gradient function w.r.t sigma     
gs<-function (x, Y, alpha, l, sigma){
  
  a = solve(k.matrix(x, alpha, l, sigma)) %*% Y
  gs <- 0.5 * matrix.trace(a%*%t(a) - solve(k.matrix(x, alpha, l, sigma)))* 2*sigma
  return (gs)
}


# initial values
theta_old <- as.vector(c(0.1, 0.1, 0.1))
theta_new <- as.vector(c(10, 10, 10))

gamma <- .2       # set the learning rate
precision <- 0.01  # set the precision
# dataset

data = read.csv("G:/My Drive/PhD/Term 7 - Spring 2018/Courses/Statistical Modeling 2/Section 4/faithful.csv", header = TRUE)
X <- data$waiting
Y  <- data$eruptions

r<-0

while((sqrt(sum(theta_new - theta_old)^2)) > precision) {
  theta_old <- theta_new
  theta_new <- theta_old - gamma * as.vector(c(ga(X, Y, theta_old[1], theta_old[2], theta_old[3]), gl(X, Y, theta_old[1], theta_old[2], theta_old[3]), gs(X, Y, theta_old[1], theta_old[2], theta_old[3])))
  r <- r+1
}


## check each part
#K <- k.matrix(X, 10.8,11.54,2.76)
#galpha <- ga(X, Y, 10.8,11.54,2.76)
#glength <- gl(X, Y, 10.8,11.54,2.76)
#gsigma <- gs(X, Y, 10.8,11.54,2.76)
#g_vect = as.vector(c(ga(X, Y, 10.8,11.54,2.76), gl(X, Y, 10.8,11.54,2.76), gs(X, Y, 10.8,11.54,2.76)))

###################################################################################
## optimizing using optim command

data = read.csv("G:/My Drive/PhD/Term 7 - Spring 2018/Courses/Statistical Modeling 2/Section 4/faithful.csv", header = TRUE)
x <- data$waiting 
y <- scale(data$eruptions)
N <- length(x)

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

optimization <- optim(par = c(0.5, 2, 0.4), fn = negative.log.likelihood, gr = NULL, method ="L-BFGS-B", x, y)
hyparam = optimization$par

#plotting part

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

x.star <- seq(0, 100, length.out = 272)

#mean and cov of fuction with optimizing values

A <- K.matrix (x, x, hyparam[1], hyparam[2], hyparam[3]) + 1* diag(1,length(x))
meanfstar <- K.matrix (x.star, x, hyparam[1], hyparam[2], hyparam[3]) %*% solve(A) %*% y
covfstar = K.matrix (x.star, x.star, hyparam[1], hyparam[2], hyparam[3]) - (K.matrix (x.star, x,  hyparam[1], hyparam[2], hyparam[3] ) %*% solve(A) %*% K.matrix (x, x.star,  hyparam[1], hyparam[2], hyparam[3]))

# 95% credible interval
lower = meanfstar - 1.96 * sqrt(diag(covfstar)) 
upper = meanfstar + 1.96 * sqrt(diag(covfstar)) 

ggplot() +
  geom_point(data = data, aes(x,y), colour = 'red', size =3) +
  labs(x = "waiting", y="eruptions")+
  geom_line(aes(x.star, meanfstar))+
  geom_ribbon(aes(x.star, ymin = lower, ymax = upper), alpha = 0.5 )