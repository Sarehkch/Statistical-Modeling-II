 // Basic Poisson glm
 
 data {
   // Define variables in data
   // Number of observations (an integer)
   int<lower=0> N;
   int<lower=0> A;
   // Covariates
   matrix[N,A] X;
   int<lower=0> y[N];
 }
 
 parameters {
   // Define parameters to estimate
   vector[A] beta;
 }
 
 model {
   // Prior part of Bayesian inference
   beta~normal(0,1);
   // Likelihood part of Bayesian inference
  y ~ poisson(exp(X*beta));
 }