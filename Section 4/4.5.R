
#Exercise 4.5

#importing data
data = read.csv("G:/My Drive/PhD/Term 7 - Spring 2018/Courses/Statistical Modeling 2/Section 4/faithful.csv", header = TRUE)
summary(data)

X <- data$waiting
y <- data$eruptions

x.star <- seq(0, 100, length.out = 200)

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


# alpha = 1, l= 1
# predictive function

A = K.matrix (1,1, X, X) + 1* diag(1,length(X))
meanfstar <- K.matrix (1,1, x.star, X) %*% solve(A) %*% y
covfstar = K.matrix (1,1, x.star, x.star) - (K.matrix (1,1, x.star, X) %*% solve(A) %*% K.matrix (1,1, X, x.star))

# 95% credible interval
 lower = meanfstar - 1.96 * sqrt(diag(covfstar)) 
 upper = meanfstar + 1.96 * sqrt(diag(covfstar)) 
 
# plotting
 ggplot() +
   geom_point(data = data, aes(data[,2],data[,1]), colour = 'red', size =3) +
   labs(x = "waiting", y="eruptions")+
   geom_line(aes(x.star, meanfstar))+
   geom_ribbon(aes(x.star, ymin = lower, ymax = upper), alpha = 0.5 ) +
   labs(title = "Graph of functions for sigma = 1, alpha =1,  l=1")
 
 
 # alpha = 3, l= 0.5
 # predictive function
 
 A = K.matrix (3,0.5, X, X) + 1* diag(1,length(X))
 meanfstar <- K.matrix (3,0.5, x.star, X) %*% solve(A) %*% y
 covfstar = K.matrix (3,0.5, x.star, x.star) - (K.matrix (3,0.5, x.star, X) %*% solve(A) %*% K.matrix (3,0.5, X, x.star))
 
 # 95% credible interval
 lower = meanfstar - 1.96 * sqrt(diag(covfstar)) 
 upper = meanfstar + 1.96 * sqrt(diag(covfstar)) 
 
 # plotting
 ggplot() +
   geom_point(data = data, aes(data[,2],data[,1]), colour = 'red', size =3) +
   labs(x = "waiting", y="eruptions")+
   geom_line(aes(x.star, meanfstar))+
   geom_ribbon(aes(x.star, ymin = lower, ymax = upper), alpha = 0.5 ) +
   labs(title = "Graph of functions for sigma = 1, alpha = 3,  l=.5")
