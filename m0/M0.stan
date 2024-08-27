//
// This Stan program defines a model for the number of checklists that a bird
// appears on in a given cell. This model uses only mean diurnal temperature
// range as a predictor.

// The input data is: occurrence data, a vector 'occ' of length 'N' (the 
// number of cells);
// climate data, given by 4 vectors ('tdr', 'ts', 'ap', 'ps') of length 'N ';
// 1

data {
  int<lower=0> N;
  // occurrence data
  int occ[N];
  int checklists[N]; // number of lists
  
  // climate data
  vector[N] ts;
}

// The parameters accepted by the model. Our model accepts coefficients for
// ts, a binomial 'n', and a binomial 'p'. 
// Assume the same regression coefficients across cells
// ie., treat them as independent draws; but allow n and p to vary depending
// on cell.
parameters {
  real coeff_ts;
  real inter;
}


// The model to be estimated. We model the output
// 'occ' to be a binomial(n, p) with n from the data and p drawn from a logistic
// regression.
model {
  // priors
  coeff_ts ~ normal(0, 10);
  inter ~ normal(0, 10);
  
  // run a logistic regression to generate p's and occurrence data
  for(i in 1:N){
    occ[i] ~ binomial_logit(checklists[i], coeff_ts*ts[i]+inter);
  }
  
}

// generated quantities {
//   // p's
//   real p[N];
//   for(i in 1:N){
//     p[i] = inv_logit(coeff_ts*ts[i]+inter);
//   }
// }

