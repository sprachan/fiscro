jan_model_obj <- readRDS('./processed_data/jan_model_obj.RDS')

model <- rstan::stan_model('M1B.stan')
occ_fit = rstan::sampling(model, 
                          jan_model_obj, 
                          iter = 5000, 
                          chains = 3,
                          cores = 3,
                          verbose = TRUE)
str(occ_fit)
saveRDS(occ_fit, file = file.path('~', 'eBird_project', 'model_outputs', 'fiscro_janfit.RDS'))