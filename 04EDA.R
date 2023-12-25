# DESCRIPTION ------------------------------------------------------------------
#>
#> Preliminary EDA. Take a random sample of the data (to reduce computational
#> burden), calculate a rough measure of density for all species, plot over
#> time. Also crow specific EDA by state to better understand data.
#>
# ------------------------------------------------------------------------------

# Load Dependencies and Data ===================================================
# easy visualizations
library(ggplot2)

# data manipulation
library(dplyr)

# color-blind friendly graphics
library(viridis)

# working with dates
library(lubridate)

load('./processed_data/combined_zf.RData')
print('loaded')

# Take a random sample of the data to lessen computational burden;
#> Also, format date data for grouping, group, and calculate observation freq.

combined_zf <- combined_zf |>
      	       slice_sample(prop = 0.25) |>
               mutate(year_mon = zoo::as.yearmon(observation_date))

# Summary by season ============================================================

combined_zf_summary <- group_by(combined_zf, species_code, year_mon) |>
                       summarize(abundance = sum(species_observed)/n()) |>
                       mutate(season = case_when(month(year_mon) %in% c(12, 1, 2) ~ 'winter',
                                                 month(year_mon) %in% c(3, 4, 5) ~ 'spring',
                                                 month(year_mon) %in% c(6, 7, 8) ~ 'summer',
                                                 month(year_mon) %in% c(9, 10, 11) ~ 'fall'),
                               season = factor(season))
print('summarized')

# plot as time series
p1 <- ggplot(combined_zf_summary, aes(x = year_mon, y = abundance, color = season))+
      geom_point(alpha = 0.75)+
      facet_wrap(facets = vars(species_code), nrow = 5, scales = 'free_y')+
      zoo::scale_x_yearmon(format = '%m/%y')+
      scale_color_manual(values = c('#db892c', '#116b0f', '#b02841', '#9cd2ff'))

ggsave('~/eBird_project/plots/all_species_time_series.png', 
       p1, 
       device='png', 
       dpi=300)


# plot as overlay, with each year on top of each other 
p2 <- ggplot(combined_zf_summary, 
             aes(x = month(year_mon), y = abundance, color = year(year_mon)))+
      geom_jitter(alpha = 0.5)+
      facet_wrap(facets = vars(species_code), scales = 'free_y', nrow = 5)+
      scale_color_viridis(option = 'H')+
      labs(color = 'year')

ggsave('~/eBird_project/plots/all_species_overlay.png', 
       p2, 
       device='png', 
       dpi=300)

# Weekly crows  ================================================================
crow_weeks <- combined_zf |> filter(species_code %in% c('fiscro', 'amecro'),
					                          year(observation_date) >= 2010) |>
                  			     mutate(week = week(observation_date),
                  				          year = year(observation_date)) |>
                             group_by(species_code, year, week, state_code) |>
                  			     summarize(crows_seen = sum(observation_count, na.rm = TRUE),
                  				             list_proportion = sum(species_observed)/n())

# check this worked
str(crow_weeks)

# plot for all states, combined
p3 <- crow_weeks |> group_by(species_code, year, week) |> 
            		    summarize(crows_seen = sum(crows_seen, na.rm = TRUE)) |>
                    ggplot(aes(x = week, y = crows_seen, color = year))+
	                  geom_jitter(alpha = 0.75)+
                    facet_wrap(facets = vars(species_code), 
                               nrow = 2, 
                               scales = 'free_y')+
                    scale_color_viridis(option = 'H')
 
##  save plot
ggsave('~/eBird_project/plots/crows_all_states.png', 
       p3, 
       device = 'png', 
       dpi = 300)

# plot american crows for each state

p4 <- crow_weeks |> filter(species_code == 'amecro') |>
                    ggplot(aes(x = week, y = crows_seen, color = year))+
	                  geom_jitter(alpha = 0.75)+
                    facet_wrap(facets = vars(state_code), 
                               nrow = 6, 
                               scales = 'free_y')+
                    scale_color_viridis(option = 'H')

#  save plot
ggsave('~/eBird_project/plots/amecro_by_state.png', 
       p4, 
       device = 'png', 
       dpi = 300)


p4.5 <- crow_weeks |> filter(species_code == 'amecro') |>
                      ggplot(aes(x = week, y = list_proportion, color = year))+
            		      geom_jitter(alpha = 0.75)+
            		      facet_wrap(facets = vars(state_code), 
            		                 nrow = 6, 
            		                 scale = 'free_y')+
            	              scale_color_viridis(option = 'H')
# save plot
ggsave('~/eBird_project/plots/amecro_prop_by_state.png', 
       p4.5, 
       device = 'png', 
       dpi = 300)	      


# plot fish crows for each state
p5 <- crow_weeks |> filter(species_code == 'fiscro') |>
                    ggplot(aes(x = week, y = crows_seen, color = year))+
                    geom_jitter(alpha = 0.75)+
                    facet_wrap(facets = vars(state_code), 
                               nrow = 6, 
                               scales = 'free_y')+
                    scale_color_viridis(option = 'H')

# save plot
ggsave('~/eBird_project/plots/fiscro_by_state.png', 
       p5, 
       device = 'png', 
       dpi = 300)

p5.5 <- crow_weeks |> filter(species_code == 'fiscro') |>
                      ggplot(aes(x = week, y = list_proportion, color = year))+
                      geom_jitter(alpha = 0.75)+
                      facet_wrap(facets = vars(state_code), 
                                 nrow = 6, 
                                 scale = 'free_y')+
                      scale_color_viridis(option = 'H') 
# save plot
ggsave('~/eBird_project/plots/fiscro_prop_by_state.png', 
       p5.5, 
       device = 'png', 
       dpi = 300)      

