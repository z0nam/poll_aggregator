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

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> n_days; // number of days
  real mu_start; // value at starting
  real mu_finish; // value at final election
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0, upper=100> mu[n_days]; // underlying state of vote intention
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  // state model
  
  mu[1] ~ normal(mu_start, 0.01);
  for (i in 2:n_days)
    mu[i] ~ normal(mu[i - 1], 0.25);
  
  // measurement model
  // 1. Election result
  
  mu[n_days] ~ normal(mu_finish, 0.01);
}

