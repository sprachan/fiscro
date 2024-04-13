library(optparse)
option_list <- list(make_option(c('-s', '--species', 
                                  action = 'store',
                                  type = 'character',
                                  help = '6 letter species code')))

opt <- parse_args(OptionParser(option_list = option_list))

if(opt$s == 'fiscro'){
  model_df <- readRDS('/m2/m2_prepped_data.RDS')$fiscro
}else if(opt$s == 'ribgul'){
  model_df <- readRDS('/m2/m2_prepped_data.RDS')$ribgul
}else if(opt$s == 'amerob'){
  model_df <- readRDS('.m2/m2_prepped_data.RDS')$amerob
}else{
  stop('have not created processed data for the requested species')
}

model_obj <- list(N = length(model_df$x),
                  K = 2,
                  occ = model_df$sum,
                  checklists = model_df$species_observed,# need to change this name at some point
                  climate = cbind(model_df$mean_temp, model_df$mean_precip)
                  ) 

model <- rstan::stan_model('m2/m2.stan')
fit = rstan::sampling(model, 
                      model_obj, 
                      iter = 5000, 
                      chains = 3,
                      cores = 3,
                      verbose = TRUE)
str(fit)
saveRDS(fit, file = file.path('~', 'eBird_project', 'model_outputs', paste0(opt$s, '_m2fit.RDS')))