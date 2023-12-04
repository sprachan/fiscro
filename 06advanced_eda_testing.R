#===============================================================================
# This is some more advanced EDA on spatially unbiased data to look at
# Species counts over space and time.
#===============================================================================

# Load dependencies and parse options ==========================================

library(tidyverse) # data manipulation tools
library(viridis) # for color-blind friendly visuals
library(zoo) # helpful for year_mon object and related tools

# Functions ====================================================================
# turn the dataframe to a matrix for easy use in lapply/maps
df_to_mat <- function(df, over, nest_by = 'ym', n = 200){
  # make sure the data frame has every lat-long combination represented
  temp <- ungroup(df)
  if(nest_by == 'ym'){
    temp <- complete(temp, nesting(year_mon),
                     long_bin = 1:n,
                     lat_bin = 1:n) |>
            filter(year_mon == over) |>
            arrange(long_bin, lat_bin)
    temp <- matrix(temp$obs_freq, nrow = n, ncol = n, byrow = TRUE)
    return(temp)
  }else if(nest_by == 'comparison'){
    temp <- complete(temp, nesting(comparison),
     		     long_bin = 1:n,
                     lat_bin = 1:n) |>		     
            filter(comparison == over) |>
            arrange(long_bin, lat_bin) 
    temp <- matrix(temp$transform_diff, nrow = n, ncol = n, byrow = TRUE)
    return(temp)
  }
}

# smooth by applying a weighted average
geom_smooth <- function(matrix_in, w = 0.75){
  N <- length(matrix_in[1,])
  out <- matrix(NA, nrow = N, ncol = N)
  for(j in 1:N){
    for(k in 1:N){
      temp <- matrix_in
      temp[j, k] <- NA
      valid_rows <- (j-1):(j+1)
      valid_rows <- valid_rows[(j-1):(j+1) >= 1 & (j-1):(j+1) <= N]
      
      valid_cols <- (k-1):(k+1)
      valid_cols <- valid_cols[(k-1):(k+1) >= 1 & (k-1):(k+1) <= N]
      neighbors <- temp[valid_rows, valid_cols]
      n <- length(neighbors)-1
      
      # if current cell is NA, do a regular average
      if(is.na(matrix_in[j, k])){
        out[j, k] <- mean(matrix_in[valid_rows, valid_cols], na.rm = TRUE)
      }else{
        # otherwise, do the weighted average
        out[j, k] <- w*matrix_in[j, k]+((1-w)/n)*sum(neighbors, na.rm = TRUE)
      }
    }
  }
  return(out)
}


# smooth the data by averaging a cell with its neighbors; does not
#> propagate NA values.
flat_smooth <- function(matrix_in){
  # assumes a square matrix (which we have)
  N <- length(matrix_in[1,]) # should be 200
  out <- matrix(NA, nrow = N, ncol = N)
  
  for(j in 1:N){
    for(k in 1:N){
      # do interior cells first
      if(1 < k && k < N && 1 < j && j < N){
        out[j, k] <- mean(matrix_in[(j-1):(j+1), (k-1):(k+1)], na.rm = TRUE)
      }else{
        valid_rows <- (j-1):(j+1)
        valid_rows <- valid_rows[(j-1):(j+1) >= 1 & (j-1):(j+1) <= N]
        
        valid_cols <- (k-1):(k+1)
        valid_cols <- valid_cols[(k-1):(k+1) >= 1 & (k-1):(k+1) <= N]
        
        out[j, k] <- mean(matrix_in[valid_rows, valid_cols], na.rm = TRUE)
      }
    }
  }
  return(out)
}

# do year-on-year comparisons of the same month in subsequent years
compare_years <- function(data_in, smooth_type){
  yms <- unique(data_in$year_mon)
  # make a data frame that has transformed differences
  x <- map(yms, ~df_to_mat(data_in, over = .x, nest_by = 'ym')) |> # get a list of 200x200 matrices, one for each year-mon
       set_names(yms) |> 
       # flatten the matrices into vectors, resulting in a list of vectors (one for each yearmon)
       lapply(t) |>
       lapply(as.vector) |> 
       # make this into a dataframe where one column is the yearmon and the other
       #> column is the observation frequency. Each yearmon has 1 row; the
       #> observation frequency cell contains a vector
       enframe(name = 'year_mon', value = 'obs_freq') |>
       # get every yearmon combination
       expand(nesting(year_mon = as.yearmon(year_mon),
                      obs_freq),
              nesting(year_mon2 = as.yearmon(year_mon),
                      obs_freq2 = obs_freq)) |>
        # only want same month, consecutive year comparisons
        filter(year(year_mon) == year(year_mon2)-1,
               month(year_mon) == month(year_mon2)) |>
        # calculate differences
        mutate(comparison = paste(year_mon, year_mon2, sep = '_'),
               diff = map2(obs_freq2, obs_freq, `-`)) |>
        select(-obs_freq, -obs_freq2, -year_mon, -year_mon2) |>
        # make it so that each element in a vector gets its own cell
        unnest_longer(diff) |>
        mutate(diff = case_when(is.nan(diff) ~ NA,
                                !is.nan(diff) ~ diff
                               ),
               transform_diff = case_when(is.na(diff) ~ NA,
                                          diff < 0 ~ -sqrt(abs(diff)),
                                          diff == 0 ~ 0,
                                          diff > 0 ~ sqrt(abs(diff))
                                          )
              )
  n <- length(unique(x$comparison))
  # add lat_bin, long_bin columns
  x <- x |> mutate(long_bin = rep(rep(1:200, each = 200), n),
                   lat_bin = rep(rep(1:200, times = 200), n))  

  str(x) # should have comparison, diff, transform_diff, long_bin, lat_bin, columns and n*200*200 rows
  
  
  com <- unique(x$comparison)
  y <- map(com, ~df_to_mat(x, over = .x, nest_by = 'comparison')) |>
       set_names(com)
  print('df to mat results')
  str(y) # should be a list of 200x200 matrices, named by the comparison and length of length(com)
  
  if(smooth_type == 'flat'){
    y <- map(y, flat_smooth)
  }else if(smooth_type == 'geom'){
    y <- map(y, geom_smooth) 
  }else{
    stop('Need valid smooth type, either flat or geom')
  }
  
  y <- set_names(y, com)
  
  print('smooth results')
  str(y) # should again be a list of 200x200 matrices, named by the comparison
  
  y <- y |> lapply(t) |>
            lapply(as.vector) |>
            # turn list of matrices into a dataframe: column 1 is comparison, column 2 is transformed difference in vectors
            enframe(name = 'comparison', value = 'transform_diff') |>
            unnest_longer(transform_diff)
  
  n <- length(unique(y$comparison))
  print(n)
  # add lat_bin, long_bin columns
  y <- y |> mutate(long_bin = rep(rep(1:200, each = 200), n),
                   lat_bin = rep(rep(1:200, times = 200), n))  
   
  str(y) 
    # reorder data: go sequentially by month within each year
  y <- y |> mutate(year_mon = as.yearmon(substring(comparison, 1, 8))) |>
		   arrange(year(year_mon), month(year_mon))
    
    # make comparison into a factor, making sure its ordered correctly
  y$comparison <- factor(y$comparison, levels = unique(y$comparison), ordered = TRUE)
  return(y)
}

# do month-on-month comparisons within a year for a given set of years
## STILL NEEDS EDITING
# compare_months <- function(data_in, years, smooth_type){
#   yms <- unique(data_in$year_mon)
#   
#   
#   if(smooth_type == 'flat'){
#     x <- map(yms, ~ df_to_mat(data_in, .x)) |>
#          map(flat_smooth)
#   }else if(smooth_type == 'geom'){
#     x <- map(yms, ~ df_to_mat(data_in, .x)) |>
#          map(geom_smooth)
#   }else{
#     stop('Need valid smooth type, either flat or geom')
#   }
#   x <- x |>
#        set_names(yms) |>
#        lapply(t) |>
#        lapply(as.vector) |>
#        enframe(name = 'year_mon', value = 'obs_freq') |> 
#     # have every year month combination
#        expand(nesting(year_mon = as.yearmon(year_mon), 
#                       obs_freq),
#               nesting(year_mon2 = as.yearmon(year_mon), 
#                       obs_freq2 = obs_freq)) |>
#     # only keep combinations in the years we want, within the same year,
#     #> in subsequent months
#     filter(year(year_mon) %in% years,
#            year(year_mon) == year(year_mon2),
#            month(year_mon) == month(year_mon2)-1) |>
#     mutate(comparison = paste(year_mon, year_mon2, sep = '_'),
#            diff = map2(obs_freq2, obs_freq, `-`)) |>
#     select(-obs_freq, -obs_freq2) |>
#     unnest_longer(diff) |>
#     mutate(diff = case_when(is.nan(diff) ~ NA,
#                             !is.nan(diff) ~ diff
#                             ),
#            transform_diff = case_when(is.na(diff) ~ NA,
#                                       diff < 0 ~ -sqrt(abs(diff)),
#                                       diff == 0 ~ 0,
#                                       diff > 0 ~ sqrt(abs(diff))
#                                      )
#           )
#   cat('month-on-month comparison df created, smoothed with', smooth_type)
#   
#   n <- length(unique(x$comparison)) 
#   print(n)
#   # add long_bin and lat_bin columns
#   x <- x |> mutate(long_bin = rep(rep(1:200, each = 200), n),
#                    lat_bin = rep(rep(1:200, times = 200), n))
#   
#   # reorder data: go sequentially by month within each year
#   x <- x |> arrange(year(year_mon), month(year_mon))
#   
#   # make comparison into a factor, making sure its ordered correctly
#   x$comparison <- factor(x$comparison, levels = unique(x$comparison), ordered = TRUE)
#   return(x)
# }
# Wrangle data =================================================================
# load data
load('./processed_data/subsample.RData')

print('loaded')

# filter data
years <- seq(2015, 2022, by = 1)
subsample <- filter(subsample, 
		                year(observation_date) %in% years,
		                species_code == 'fiscro')
print('filtered')

# process data
ym_obs_freq <- mutate(subsample,
		                  year_mon = as.yearmon(observation_date)) |>
	                    group_by(year_mon, long_bin, lat_bin) |>
	                    summarize(obs_freq = sum(species_observed)/n())
print('summarized')

# free up some RAM
remove(subsample)




# Flat Smoothing ===============================================================

yms <- unique(ym_obs_freq$year_mon)

## Year-on-Year comparisons ----
yy_compare_flat <- compare_years(ym_obs_freq, smooth_type = 'flat')
str(yy_compare_flat)

flat_plot <- yy_compare_flat |> filter(comparison == 'Jan 2019_Jan 2020') |>
                   ggplot()+
                   geom_raster(aes(x = long_bin, y = lat_bin, fill = transform_diff))+
                   scale_fill_distiller(palette = 'RdBu', direction = -1, na.value = '#cccccc')

fp <- file.path('~', 'eBird_project', 'plots', 'test_flat_yy')
pdf(paste0(fp, '.pdf'), width = 11, height = 8.5)
print(flat_plot)
dev.off()

# Geometric Smoothing ==========================================================

## Year-on-Year comparisons ----
yy_compare_geom <- compare_years(ym_obs_freq, smooth_type = 'geom')
str(yy_compare_geom)

geom_plot <- yy_compare_geom |> filter(comparison == 'Jan 2019_Jan 2020') |>
  ggplot()+
  geom_raster(aes(x = long_bin, y = lat_bin, fill = transform_diff))+
  scale_fill_distiller(palette = 'RdBu', direction = -1, na.value = '#cccccc')

fp <- file.path('~', 'eBird_project', 'plots', 'test_geom_yy')
pdf(paste0(fp, '.pdf'), width = 11, height = 8.5)
print(geom_plot)
dev.off()
