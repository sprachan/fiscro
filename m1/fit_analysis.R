# get parameters and data ----
params <- rstan::extract(readRDS('../data/fiscro_M0_tdr_fit.RDS'))
params <- list(coeff_tdr = params$coeff_tdr, inter = params$inter)
rstan::summary(readRDS('../data/fiscro_M0_tdr_fit.RDS'))$summary # ESS's look good!
data_in <- readRDS('../data/fiscro_tdr_data.RDS') # PUT CELL IDS IN HERE

p <- rep(NA, length(params$coeff_tdr))
temp <- rep(NA, length(data_in$tdr))

# I will generate average p from each iteration: 
for(i in 1:length(params$coeff_tdr)){
  temp <- 1/(1+exp(-params$coeff_tdr[i]*data_in$tdr-params$inter[i]))
  p[i] <- mean(temp) # take geometric mean if I do it this way
  #> use pbinom for tail probabilities from this
}
rm(temp)
# visualize parameter distributions ----
hist(params$coeff_tdr, freq = FALSE)
hist(params$inter, freq = FALSE)
hist(p, freq = FALSE)

# 95% credible intervals ----
coeff_ci <- quantile(params$coeff_tdr, c(0.025, 0.975)) 
# see what that means for odds
exp(coeff_ci) 
# so increasing the temperature diurnal range by 1 means an odds ratio of 1.03
#> that's quite close to odds ratio of 1

inter_ci <- quantile(params$inter, c(0.025, 0.975))
# change into odds
exp(inter_ci) # if tdr is at 0 (no variation), the odds ratio is about 0.1 --
#> this is having not observed a good deal more likely than observed

p_ci <- quantile(p, c(0.025, 0.975)) # this is very tight: 0.125

# posterior predictive distribution -----
N <- 1e3
# will sample parameters from the posterior
sample_params <- lapply(params, sample, size = N, replace = TRUE)

# draw responses for each parameter. This gives us a distribution of 1000 possible
#> data sets
responses <- matrix(NA, nrow = N, ncol = length(data_in$tdr))
for(i in 1:N){
  responses[i,] <- rbinom(length(data_in$tdr),
                          size = data_in$checklists, 
                          prob = 1/(1+exp(sample_params$coeff_tdr[i]*data_in$tdr+sample_params$inter[i])))
}

# posterior predictive checks for mean and sd
## mean
means <- apply(responses-1, MARGIN = 1, FUN = mean) # because I used occ+1 to fit the model
hist(means, freq = FALSE, n = 100, xlim = c(0.2, 4.5))
abline(v = mean(data_in$occ), lwd = 2) # does not look good...over predicting

## sd
sds <- apply(responses-1, MARGIN = 1, FUN = sd)
hist(sds, freq = FALSE, n = 100)
abline(v = sd(data_in$occ)) # this is even worse...sd ~ 2 for data but ~ 16.9 for model!

