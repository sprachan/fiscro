//
// This Stan program defines a model for the number of checklists that a bird
// appears on in a given cell. This model uses only latitude as a predictor.

// The input data is: occurrence data, a vector 'occ' of length 'N' (the 
// number of cells), and latitude.

data {
  int<lower=0> N;
  // occurrence data
  int occ[N];
  int checklists[N]; // number of lists
  
  // climate data
  vector[N] lat;
}

// The parameters accepted by the model. Our model accepts coefficients for
// ts, a binomial 'n', and a binomial 'p'. 
// Assume the same regression coefficients across cells
// ie., treat them as independent draws; but allow n and p to vary depending
// on cell.
parameters {
  real coeff_lat;
  real inter;
}


// The model to be estimated. We model the output
// 'occ' to be a binomial(n, p) with n from the data and p drawn from a logistic
// regression.
model {
  // priors
  coeff_lat ~ cauchy(0, 2.5);
  inter ~ cauchy(0, 2.5);
  
  // run a logistic regression to generate p's and occurrence data
  occ ~ binomial_logit(checklists, coeff_lat*lat+inter);
}


