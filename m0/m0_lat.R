library(optparse)
option_list <- list(make_option(c('-s', '--species', 
                                  action = 'store',
                                  type = 'character',
                                  help = '6 letter species code')))

opt <- parse_args(OptionParser(option_list = option_list))

# can use m2 prepped data
if(opt$s == 'fiscro'){
  model_df <- readRDS('m2/m2_prepped_data.RDS')$fiscro
}else if(opt$s == 'ribgul'){
  model_df <- readRDS('m2/m2_prepped_data.RDS')$ribgul
}else if(opt$s == 'amerob'){
  model_df <- readRDS('m2/m2_prepped_data.RDS')$amerob
}else{
  stop('have not created processed data for the requested species')
}

# make this into a list
model_obj <- list(long = model_df$x,
                  lat = model_df$y,
                  occ = model_df$sum,
                  checklists = model_df$species_observed,
                  N = length(model_df$x))

# fit model
model <- rstan::stan_model('m0/m0_lat.stan')
fit = rstan::sampling(model, 
                      model_obj, 
                      iter = 5000, 
                      chains = 3,
                      cores = 3,
                      verbose = TRUE)
str(fit)
saveRDS(fit, file = file.path('~', 'eBird_project', 'model_outputs', paste0(opt$s, '_m0lat_fit.RDS')))
