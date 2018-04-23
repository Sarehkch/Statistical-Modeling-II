
#Exercise 4.2

#importing data
data = read.csv("G:/My Drive/PhD/Term 7 - Spring 2018/Courses/Statistical Modeling 2/Section 4/faithful.csv", header = TRUE)
summary(data)

#install.packages("ggplot2")
#library(ggplot2)
#library(MASS)

# plotting data
ggplot(data = data) +
  geom_point(aes(data[,2],data[,1]), colour = 'red', size =3) +
  labs(x = "waiting", y="eruptions")

#Model

X <- data$waiting
y <- data$eruptions

x.star <- seq(min(X), max(X), length.out = 50)

phi <- matrix(1, nrow = length(X), ncol =4 )
phi[,2] <- X
phi[,3] <- X^2
phi[,4] <- X^3

phi.xstar <- matrix(1, nrow =50 , ncol =4 )
phi.xstar[,2] <- x.star
phi.xstar[,3] <- x.star ^2
phi.xstar[,4] <- x.star^3

#posterior distribution of function

# sigman = 1
sigma <- diag(1, 4)
A <- t(phi)%*% phi + solve(sigma)

mean <- phi.xstar %*% solve(A)%*% t(phi) %*% y 
cov.matrix <- phi.xstar %*% solve(A)%*% t(phi.xstar)

# 95% credible interval
lower <- mean - 1.96*sqrt(diag(cov.matrix))
upper <- mean + 1.96*sqrt(diag(cov.matrix))

# plotting function
ggplot() +
  geom_point(data = data, aes(data[,2],data[,1]), colour = 'red', size =3) +
  labs(x = "waiting", y="eruptions")+
  geom_line(aes(x.star, mean))+
  geom_ribbon(aes(x.star, ymin = lower, ymax = upper), alpha = 0.5 )
