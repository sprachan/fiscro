library(ggplot2)
fit <- readRDS('../data/fiscro_M1B_fit.RDS')
coeffs <- rstan::extract(fit)$coeffs
colnames(coeffs) <- c('water', 
                      'developed', 
                      'barren', 
                      'forest', 
                      'shrub', 
                      'grassland', 
                      'planted_cultivated', 
                      'wetlands')


coeff_df <- as.data.frame(coeffs) |>
            tidyr::pivot_longer(cols = c('water', 'developed', 'barren', 'forest', 'shrub', 'grassland', 'planted_cultivated', 'wetlands'),
                                cols_vary = 'slowest',
                                names_to = 'lc_type',
                                values_to = 'coeff')

ggplot(coeff_df, aes(x = coeff))+
  geom_histogram(color = 'darkgrey', fill = 'pink', bins = 40)+
  facet_wrap(facets = vars(lc_type), scales = 'free')+
  theme_bw()

# credible intervals
ci <- coeff_df |> dplyr::group_by(lc_type) |> 
                  dplyr::summarize(ci_high = quantile(coeff, 0.025),
                                   ci_low = quantile(coeff, 0.975))
df <- dplyr::full_join(coeff_df, ci, by = dplyr::join_by(lc_type))
 
ggplot(df, aes(x = coeff))+
  geom_histogram(color = '#000004FF', fill = '#BB3754FF', bins = 40)+
  geom_vline(aes(xintercept = ci_high))+
  geom_vline(aes(xintercept = ci_low))+
  facet_wrap(facets = vars(lc_type), scales = 'free')+
  theme_bw()
