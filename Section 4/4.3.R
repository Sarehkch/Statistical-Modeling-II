
#Exercise 4.3

#squared exponential covariance function,

K.matrix <- function (alpha, l, X1, X2) {
  
  K <- matrix(rep(0, length(X1)*length(X2)), nrow = length(X1))
  
  for (i in 1:nrow(K)){
    for (j in 1:ncol(K)) {
      K[i,j] <- (alpha^2) * exp(-0.5*(abs(X1[i] - X2[j])/l)^2)
    }
  }
  return(K)
}

X <- seq(0, 100, len = 200)

# alpha = 1, l= 1
K <- K.matrix (1,1, X, X)

## sample function
library(mvtnorm)

f = matrix(0, nrow = length(X), ncol = 5)
for (i in 1:5){
  f[,i] <- rmvnorm(1, mean = rep(0, length(X)), sigma = K)
}

##ploting
library(ggplot2)
library(reshape2)
df <- data.frame(X, as.data.frame(f))
df <- melt(df ,  id.vars = 'X', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(X,value)) + geom_line(aes(colour = series))+
  labs(title = "Graph of functions for l=1")

############################################
# alpha = 1, l= 0.1
K <- K.matrix (1,0.1, X, X)

## sample function
library(mvtnorm)

f = matrix(0, nrow = length(X), ncol = 5)
for (i in 1:5){
  f[,i] <- rmvnorm(1, mean = rep(0, length(X)), sigma = K)
}

##ploting
library(ggplot2)
library(reshape2)
df <- data.frame(X, as.data.frame(f))
df <- melt(df ,  id.vars = 'X', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(X,value)) + geom_line(aes(colour = series))+
  labs(title = "Graph of functions for l=0.1")

############################################
# alpha = 1, l= 10
K <- K.matrix (1,10, X, X)

## sample function
library(mvtnorm)

f = matrix(0, nrow = length(X), ncol = 5)
for (i in 1:5){
  f[,i] <- rmvnorm(1, mean = rep(0, length(X)), sigma = K)
}

##ploting
library(ggplot2)
library(reshape2)
df <- data.frame(X, as.data.frame(f))
df <- melt(df ,  id.vars = 'X', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(X,value)) + geom_line(aes(colour = series))+
  labs(title = "Graph of functions for l=10")