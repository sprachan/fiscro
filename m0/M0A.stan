//
// This Stan program defines a model for the number of checklists that a bird
// appears on in a given cell. This model uses only climate data as predictors.

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
  vector[N] tdr;
  vector[N] ts;
  vector[N] ap;
  vector[N] ps;
  
  // unused here, but to check loading the data into STAN: land cover
  // vector[N] lc;
}

// The parameters accepted by the model. Our model accepts coefficients for
// each climate variable (coeff_tdr, etc.), a binomial 'n',
// and a binomial 'p'. Assume the same regression coefficients across cells
// ie., treat them as independent draws; but allow n and p to vary depending
// on cell.
parameters {
  real coeff_tdr;
  real coeff_ts;
  real coeff_ap;
  real coeff_ps;
}


// The model to be estimated. We model the output
// 'occ' to be a binomial(n, p) with n from the data and p drawn from a logistic
// regression.
model {
  real m[N];
  // priors
  coeff_tdr ~ normal(0, 10);
  coeff_ts ~ normal(0, 10);
  coeff_ap ~ normal(0, 10);
  coeff_ps ~ normal(0, 10);
  
  // run a logistic regression to generate the p that controls the occurrence data
  for(i in 1:N){
    occ[i] ~ binomial_logit(checklists[i], coeff_tdr*tdr[i]+coeff_ts*ts[i]+coeff_ap*ap[i]+coeff_ps*ps[i]);
  }
  
}




