/*
This Stan program defines a model for the number of checklists that a bird
appears on in a given cell. 
This model uses only meteorological data (mean temperature and precipitation)
as predictors. Cells are treated as independent and time is not included
in the model.

The input data is occurrence data, a vector 'occ' of length 'N' (the 
 number of defined observations), and climate data (vectors of length 'N')

*/
data {
  int<lower=0> N; // length of data
  int<lower=0> K; // number of predictors
  int occ[N]; // observed outcomes
  int checklists[N]; // observed number of lists
  
  // climate
  matrix [N, K] climate;
}

// The parameters accepted by the model. 
parameters {
  real inter; // intercept
  vector[K] coeffs; // coefficients
}

/* The model to be estimated. We model the output
'occ' to be a binomial(n, p) with n from the data and p drawn from a 
 categorical logistic regression.*/
model {
  // priors
  coeffs[1] ~ cauchy(0, 2.5); // Gelman's weakly informative prior for all
  coeffs[2] ~ cauchy(0, 2.5);
  inter ~ cauchy(0, 2.5); 
  // run a logistic regression to generate the p that controls the occurrence data
  occ ~ binomial_logit(checklists, climate*coeffs+inter);
}




