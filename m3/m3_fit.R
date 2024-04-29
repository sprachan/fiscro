library(optparse)
option_list <- list(make_option(c('-s', '--species',
                                  action = 'store',
                                  type = 'character',
                                  help = '6 letter species code')))

opt <- parse_args(OptionParser(option_list = option_list))

model_df <- readRDS(paste0('m3_prep_', s, '.RDS'))[-1,]

# get data lengths ---- 
lengths_df <- dplyr::group_by(model_df, day) |> 
              dplyr::summarize(count = dplyr::n())
group_size <- lengths_df$count
rm(lengths_df)

# recode land cover covariates, standardize ----
# AND, following Gelman, shift binary inputs to have mean 0 and SD 1,
# and shift continuous variables centered at 0 with sd 0.5

model_df <- dplyr::mutate(model_df,
                          water = dplyr::case_when(modal_lc == 10 ~ 1,
                                                   .default = 0),
                          water = water - mean(water),
                          
                          developed = dplyr::case_when(modal_lc == 20 ~ 1,
                                                       .default = 0),
                          developed = developed - mean(developed),
                        
                          barren = dplyr::case_when(modal_lc == 30 ~ 1,
                                                    .default = 0),
                          barren = barren - mean(barren),
                        
                          forest = dplyr::case_when(modal_lc == 40 ~ 1,
                                                    .default = 0),
                          forest = forest - mean(forest),
                          
                          shrub = dplyr::case_when(modal_lc == 50 ~ 1,
                                                   .default = 0),
                          shrub = shrub - mean(shrub),
                        
                          grassland = dplyr::case_when(modal_lc == 70 ~ 1,
                                                       .default = 0),
                          grassland = grassland - mean(grassland),
                         
                          planted_cultivated = dplyr::case_when(modal_lc == 80 ~ 1,
                                                                 .default = 0),
                          planted_cultivated = planted_cultivated - mean(planted_cultivated),
                         
                          wetlands = dplyr::case_when(modal_lc == 90 ~ 1,
                                                      .default = 0),
                          wetlands = wetlands - mean(wetlands),
                         
                          transform_precip = (precip - mean(precip))/(2*sd(precip)),
                          transform_tmean = (tmean - mean(tmean))/(2*sd(tmean))
)

# make model object ----
model_obj <- list(N = length(model_df$day),
                  K = 10,
                  obs = model_df$num_obs,
                  lists = model_df$num_lists,
                  group_size = group_size,
                  predictors = cbind(model_df$precip,
                                     model_df$tmean,
                                     model_df$water,
                                     model_df$developed,
                                     model_df$barren,
                                     model_df$forest,
                                     model_df$shrub,
                                     model_df$grassland,
                                     model_df$planted_cultivated,
                                     model_df$wetlands))
str(model_obj)
model <- rstan::stan_model('m3/m3.stan')
fit = rstan::sampling(model, 
                      model_obj, 
                      iter = 5000, 
                      chains = 4,
                      cores = 4,
                      verbose = TRUE)
saveRDS(fit, file = file.path('~', 'eBird_project', 'model_outputs', paste0(opt$s, '_m3fit.RDS')))