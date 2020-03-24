//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=1> n_days;            // number of days
  real mu_start;                  // value at starting election
  real mu_finish;                 // value at final election
  int<lower=1> y_n;               // number of polls
  real y_values[y_n];             // actual values in polls
  int<lower=0> y_days[y_n];       // the number of days since starting election each poll was taken
  real y_se[y_n];
}
parameters {
  real<lower=0,upper=100> mu[n_days];               // underlying state of vote intention
}

model {
  
  // state model
  mu[1] ~ normal(mu_start, 0.09);
  for (i in 2:n_days) 
      mu[i] ~ normal(mu[i - 1], 5);
      
  // measurement model
  // 1. Election result
  mu[n_days] ~ normal(mu_finish, 0.09);
  
  // 2. Polls
  for(t in 1:y_n)
      y_values[t] ~ normal(mu[y_days[t]], y_se[t]);
  
}
