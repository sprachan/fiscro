# SETUP -- RUN FIRST ----
library(ggplot2)
library(viridis)
library(patchwork)
species <- 'ribgul' # change as needed
if(species == 'fiscro'){
  species_full <- 'Fish Crow'
}else if(species == 'ribgul'){
  species_full <- 'Ring-Billed Gull'
}else if(species == 'amerob'){
  species_full <- 'American Robin'
}

fit_file <- paste0('./', species, '_m2fit.RDS')


# analysis -----
# get parameters and make them into a dataframe
post_df <- data.frame(beta = c(rstan::extract(readRDS(fit_file))$inter, 
                               rstan::extract(readRDS(fit_file))$coeffs[,1],
                               rstan::extract(readRDS(fit_file))$coeffs[,2]),
                      type = c(rep('intercept', 7500), 
                               rep('temp_coeff', 7500),
                               rep('precip_coeff', 7500)))
inter <- post_df$beta[post_df$type == 'intercept']
temp_coeff <- post_df$beta[post_df$type == 'temp_coeff']
precip_coeff <- post_df$beta[post_df$type == 'precip_coeff']  

# credible intervals 
ci <- post_df |> dplyr::group_by(type) |>
                 dplyr::summarize(upper = quantile(beta, 0.975),
                                  lower = quantile(beta, 0.025))
print(ci)
post_df <- dplyr::full_join(post_df, ci, by = dplyr::join_by(type))

# visualize
hists <- ggplot(data = post_df, aes(x = beta))+
         geom_histogram(fill = '#dddddd', color = '#000000', bins = 50)+
         geom_vline(aes(xintercept = upper), col = 'red', linetype = 'dashed')+
         geom_vline(aes(xintercept = lower), col = 'red', linetype = 'dashed')+
         #geom_vline(aes(xintercept = 0), col = 'blue')+
         facet_wrap(facets = vars(type), 
                    nrow = 3,
                    scales = 'free',
                    labeller = labeller(type = c(intercept = 'Intercept',
                                                 precip_coeff = 'Precipitation',
                                                 temp_coeff = 'Temperature ')))+
         labs(title = paste(species_full, 'Posterior Distributions'),
              x = 'Value',
              y = 'Count')
         
hists        
#ggsave(filename = paste0(species, '_post.png'), device = 'png')

# "best fit" p's for each cell -----
# 'best fit' p for intercept
1/(1+exp(-mean(inter)))

if(species == 'fiscro'){
model_df <- readRDS('m2_prepped_data.RDS')$fiscro
}else if(species == 'ribgul'){
  model_df <- readRDS('m2_prepped_data.RDS')$ribgul
}else if(species == 'amerob'){
  model_df <- readRDS('m2_prepped_data.RDS')$amerob
}else{
  stop('have not created processed data for the requested species')
}

# now use the mean of the posterior distributions to calculate "best fit" p's for each cell
model_df <- dplyr::mutate(model_df,
                          lin = mean(precip_coeff)*mean_precip +
                                mean(temp_coeff)*mean_temp+
                                mean(inter),
                          p = 1/(1+exp(-lin)))
hist(model_df$p)

base_logp_plot <- ggplot(model_df, aes(x = x, y = y))+
               theme(legend.key.size = unit(0.25, 'cm'),
                     legend.text = element_text(size = rel(0.5)),
                     legend.title = element_text(size = rel(0.75)),
                     legend.position = 'top')+
               scale_fill_viridis(option = 'inferno',
                                  limits = c(-3, 0),
                                  breaks = c(-3, -2, -1, 0))

base_p_plot <- ggplot(model_df, aes(x = x, y = y))+
               theme(legend.key.size = unit(0.25, 'cm'),
                     legend.text = element_text(size = rel(0.5)),
                     legend.title = element_text(size = rel(0.75)),
                     legend.position = 'top')+
               scale_fill_viridis(option = 'inferno')

precip_plot <- ggplot(model_df, aes(x = x, y = y, fill = mean_precip))+
               geom_tile()+
               theme(legend.key.size = unit(0.25, 'cm'),
                     legend.text = element_text(size = rel(0.5)),
                     legend.title = element_text(size = rel(0.75)),
                     legend.position = 'bottom')+
               scale_fill_viridis(option = 'inferno')

temp_plot <- ggplot(model_df, aes(x = x, y = y, fill = mean_temp))+
               geom_tile()+
               theme(legend.key.size = unit(0.25, 'cm'),
                     legend.text = element_text(size = rel(0.5)),
                     legend.title = element_text(size = rel(0.75)),
                     legend.position = 'bottom')+
               scale_fill_viridis(option = 'inferno')

# let legend scales vary
(base_p_plot+ geom_tile(aes(fill = log10(p+1e-3)))
  | base_p_plot+geom_tile(aes(fill = log10(obs_freq+1e-3)))
)/(precip_plot | temp_plot)

# make sure that the predicted and observed values have the same obs freq
(base_logp_plot+ geom_tile(aes(fill = log10(p+1e-3))) 
  | base_logp_plot+geom_tile(aes(fill = log10(obs_freq+1e-3)))
)/(precip_plot | temp_plot)

# ggsave(filename = paste0(species, '_bestfit.png'), device = 'png')