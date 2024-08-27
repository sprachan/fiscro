library(ggplot2)
jan_fit <- readRDS('../data/fiscro_janfit.RDS')
jun_fit <- readRDS('../data/fiscro_junfit.RDS')

jan_model_obj <- readRDS('~/Desktop/jan_model_obj.RDS')
jun_model_obj <-  readRDS('~/Desktop/jun_model_obj.RDS')

jun_params <- cbind(rstan::extract(jun_fit)$coeffs, rstan::extract(jun_fit)$inter)
colnames(jan_params) <- c('Water', 
                      'Developed', 
                      'Barren', 
                      'Forest', 
                      'Shrub', 
                      'Grassland', 
                      'Planted_Cultivated', 
                      'Wetlands',
                      'Intercept')
colnames(jun_params) <- colnames(jan_params)

jan_param_df <- as.data.frame(jan_params) |>
                tidyr::pivot_longer(cols = colnames(jan_params),
                                    cols_vary = 'slowest',
                                    names_to = 'lc_type',
                                    values_to = 'value')

jun_param_df <- as.data.frame(jun_params) |>
  tidyr::pivot_longer(cols = colnames(jun_params),
                      cols_vary = 'slowest',
                      names_to = 'lc_type',
                      values_to = 'value')

# credible intervals
jan_ci <- jan_param_df |> dplyr::group_by(lc_type) |> 
          dplyr::summarize(ci_high = quantile(value, 0.025),
                           ci_low = quantile(value, 0.975))
jan_df <- dplyr::full_join(jan_param_df, jan_ci, by = dplyr::join_by(lc_type))
jan_df$lc_type <- factor(jan_df$lc_type, levels = colnames(jan_params))

jun_ci <- jun_param_df |> dplyr::group_by(lc_type) |> 
  dplyr::summarize(ci_high = quantile(value, 0.025),
                   ci_low = quantile(value, 0.975))
jun_df <- dplyr::full_join(jun_param_df, jun_ci, by = dplyr::join_by(lc_type))
jun_df$lc_type <- factor(jun_df$lc_type, levels = colnames(jun_params))

combined <- dplyr::bind_rows(jun_df, jan_df, .id = 'month') |>
            dplyr::mutate(month = dplyr::case_when(month == '1' ~ 'Jun',
                                                   month == '2' ~ 'Jan'))

  
ggplot(combined, aes(x = value, fill = month))+
  geom_histogram(color = '#000004FF', bins = 40)+
  geom_vline(aes(xintercept = ci_high))+
  geom_vline(aes(xintercept = ci_low))+
  facet_grid(rows = vars(lc_type), cols = vars(month), scales = 'free')+
  theme_bw()

ggplot(combined, aes(x = value))+
  geom_histogram(color = '#000004FF', fill = '#dedede', bins = 40)+
  geom_vline(aes(xintercept = ci_high), linetype = 'dotted')+
  geom_vline(aes(xintercept = ci_low), linetype = 'dotted')+
  geom_vline(aes(xintercept = 0))+
  facet_grid(cols = vars(lc_type), rows = vars(month), scales = 'free')+
  theme_bw()
ggsave('~/Desktop/coeffs.png', device = 'png', dpi = 1000, width = 24, height = 5, units = 'in')

ggplot(jan_df, aes(x = value))+
  geom_histogram(color = '#000004FF', fill = '#dedede', bins = 40)+
  geom_vline(aes(xintercept = ci_high))+
  geom_vline(aes(xintercept = ci_low))+
  geom_vline(aes(xintercept = 0), color = 'red')+
  facet_wrap(facets = vars(lc_type), scales = 'free')+
  theme_bw()+
  theme(strip.text = element_text(face = 'bold'))+
  labs(x = 'Value', y = 'Count', title = 'January')
ggsave('~/Desktop/jan_coeffs.png', device = 'png', dpi = 'retina', width = 12, height = 10, units = 'in')

ggplot(jun_df, aes(x = value))+
  geom_histogram(color = '#000004FF', fill = '#dedede', bins = 40)+
  geom_vline(aes(xintercept = ci_high))+
  geom_vline(aes(xintercept = ci_low))+
  geom_vline(aes(xintercept = 0), color = 'red')+
  facet_wrap(facets = vars(lc_type), scales = 'free')+
  theme_bw()+
  theme(strip.text = element_text(face = 'bold'))+
  labs(x = 'Value', y = 'Count', title = 'June')
ggsave('~/Desktop/jun_coeffs.png', device = 'png', dpi = 'retina', width = 12, height = 10, units = 'in')

# estimate for intercept p
1-(1/(1+exp(mean(jan_params[,9])))) # this is low! mean p is actually 0.02

# june mean is 0.04
1-(1/(1+exp(mean(jun_params[,9])))) # this is a little bit low: 0.035 vs 0.04
   
# Project some data =====

landcover <- terra::rast(file.path(env_dir, 'land_cover_final.tif'))

# reclassify to just 8 classes
classify_mat <- matrix(c(11, 10,
                         21, 20,
                         22, 20,
                         23, 20,
                         24, 20,
                         31, 30,
                         41, 40,
                         42, 40,
                         43, 40,
                         52, 50,
                         71, 70,
                         81, 80,
                         82, 80,
                         90, 90,
                         95, 90), 
                       ncol = 2,
                       byrow = TRUE) 
landcover_simple <- terra::classify(landcover, rcl = classify_mat, others = NA)
levels(landcover_simple) <- data.frame(index = c(10, 20, 30, 40, 50, 70, 80, 90), 
                                       value = c('water', 
                                                 'developed', 
                                                 'barren', 
                                                 'forest',
                                                 'shrub', 
                                                 'grassland', 
                                                 'planted_cultivated', 
                                                 'wetland'))   
mean_jun_coeffs <- apply(jun_params, MARGIN = 2, FUN = mean, na.rm = TRUE)
mean_jan_coeffs <- apply(jan_params, MARGIN = 2, FUN = mean, na.rm = TRUE)
lc_df <- terra::as.data.frame(landcover_simple, xy = TRUE) |>
         dplyr::mutate(water = as.numeric(value == 'water'),
                       developed = as.numeric(value == 'developed'),
                       barren = as.numeric(value == 'barren'),
                       forest = as.numeric(value == 'forest'),
                       shrub = as.numeric(value == 'shrub'),
                       grassland = as.numeric(value == 'grassland'),
                       planted_cultivated = as.numeric(value == 'planted_cultivated'),
                       wetland = as.numeric(value == 'wetland'),
                       
                       jun_lin = mean_jun_coeffs[1]*water+
                                  mean_jun_coeffs[2]*developed+
                                  mean_jun_coeffs[3]*barren+
                                  mean_jun_coeffs[4]*forest+
                                  mean_jun_coeffs[5]*shrub+
                                  mean_jun_coeffs[6]*grassland+
                                  mean_jun_coeffs[7]*planted_cultivated+
                                  mean_jun_coeffs[8]*wetland+
                                  mean_jun_coeffs[9],
                       jun_p = 1-1/(1+exp(jun_lin)),
                       
                       jan_lin = mean_jan_coeffs[1]*water+
                         mean_jan_coeffs[2]*developed+
                         mean_jan_coeffs[3]*barren+
                         mean_jan_coeffs[4]*forest+
                         mean_jan_coeffs[5]*shrub+
                         mean_jan_coeffs[6]*grassland+
                         mean_jan_coeffs[7]*planted_cultivated+
                         mean_jan_coeffs[8]*wetland+
                         mean_jan_coeffs[9],
                       jan_p = 1-1/(1+exp(jan_lin)))
lc_df$jun_pred <- rbinom()

sp::coordinates(lc_df) <- ~x+y
lc_df <- terra::vect(lc_df)
terra::plot(terra::rasterize(lc_df, landcover_simple, field = 'jun_p'))
