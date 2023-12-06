# Plotting Functions ===========================================================
save_pages <- function(ggobj, type, directory, ncol, nrow, species, facets){
  all_plots <- lapply(1:ggforce::n_pages(ggobj), function(j){
    p_save <- ggobj+ggforce::facet_wrap_paginate(facets = facets,
                                                 ncol = ncol,
                                                 nrow = nrow,
                                                 page = j)
    return(p_save)
  }
  )
  
  name <- paste0(species, '_', type, '.pdf')
  
  fp <- file.path('~', 'eBird_project', 'plots', directory, species, name)
  pdf(fp, width = 11, height = 8.5)
  lapply(all_plots, print)
  dev.off()
}

# Smoothing Functions ==========================================================
# smooth by applying a weighted average
geom_smooth <- function(matrix_in, w = 0.75, scope = 1){
  N <- length(matrix_in[1,])
  out <- matrix(NA, nrow = N, ncol = N)
  for(j in 1:N){
    for(k in 1:N){
      temp <- matrix_in
      a = j-scope
      b = j+scope
      temp[j, k] <- NA
      valid_rows <- a:b
      valid_rows <- valid_rows[a:b >= 1 & a:b <= N]
      
      valid_cols <- (k-1):(k+1)
      valid_cols <- valid_cols[a:b >= 1 & a:b <= N]
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


# smooth the data by averaging a cell with its neighbors. Does not propagate
# NA values unless everything within <scope> cells is NA.
flat_smooth <- function(matrix_in, scope = 1){
  # assumes a square matrix (which we have)
  N <- length(matrix_in[1,]) # should be 200
  out <- matrix(NA, nrow = N, ncol = N)
  
  for(j in 1:N){
    for(k in 1:N){
      a <- j-scope
      b <- j+scope
      valid_rows <- a:b
      valid_rows <- valid_rows[a:b >= 1 & a:b <= N]
      
      a <- k-scope
      b <- k+scope
      valid_cols <- a:b
      valid_cols <- valid_cols[a:b >= 1 & a:b <= N]
      
      out[j, k] <- mean(matrix_in[valid_rows, valid_cols], na.rm = TRUE)
    }
  }
  return(out)
}

# Data Manipulation Functions ==================================================
# turn the dataframe to a matrix for easy use in lapply/maps
df_to_mat <- function(df, over, nest_by = 'ym', n = 200){
  temp <- ungroup(df)
  if(nest_by == 'ym'){
    temp <- complete(temp, 
                     nesting(year_mon),
                     long_bin = 1:n,
                     lat_bin = 1:n) |>
      filter(year_mon == over) |>
      arrange(long_bin, lat_bin)
    temp <- matrix(temp$obs_freq, nrow = n, ncol = n, byrow = TRUE)
    return(temp)
  }else if(nest_by == 'comparison'){
    temp <- complete(temp, 
                     nesting(comparison),
                     long_bin = 1:n,
                     lat_bin = 1:n) |>		     
      filter(comparison == over) |>
      arrange(long_bin, lat_bin) 
    temp <- matrix(temp$transform_diff, nrow = n, ncol = n, byrow = TRUE)
    return(temp)
  }
}

# Comparison functions ---------------------------------------------------------
compare_years <- function(data_in, smooth_type){
  yms <- unique(data_in$year_mon)
  # make a data frame that has transformed differences
  x <- map(yms, ~df_to_mat(data_in, over = .x, nest_by = 'ym')) |> 
    set_names(yms) |> 
    lapply(t) |>
    lapply(as.vector) |> 
    enframe(name = 'year_mon', value = 'obs_freq') |>
    expand(nesting(year_mon = as.yearmon(year_mon),
                   obs_freq),
           nesting(year_mon2 = as.yearmon(year_mon),
                   obs_freq2 = obs_freq)) |>
    filter(year(year_mon) == year(year_mon2)-1,
           month(year_mon) == month(year_mon2)) |>
    mutate(comparison = paste(year_mon, year_mon2, sep = '_'),
           diff = map2(obs_freq2, obs_freq, `-`)) |>
    select(-obs_freq, -obs_freq2, -year_mon, -year_mon2) |>
    unnest_longer(diff) |>
    mutate(diff = case_when(is.nan(diff) ~ NA,
                            !is.nan(diff) ~ diff),
           transform_diff = case_when(is.na(diff) ~ NA,
                                      diff < 0 ~ -sqrt(abs(diff)),
                                      diff == 0 ~ 0,
                                      diff > 0 ~ sqrt(abs(diff)))
    )
  
  n <- length(unique(x$comparison))
  # add lat_bin, long_bin columns
  x <- x |> mutate(long_bin = rep(rep(1:200, each = 200), n),
                   lat_bin = rep(rep(1:200, times = 200), n))  
  
  com <- unique(x$comparison)
  y <- map(com, ~df_to_mat(x, over = .x, nest_by = 'comparison')) |>
    set_names(com)
  if(smooth_type == 'flat'){
    y <- map(y, flat_smooth)
  }else if(smooth_type == 'geom'){
    y <- map(y, geom_smooth) 
  }else{
    stop('Need valid smooth type, either flat or geom')
  }
  
  y <- set_names(y, com) |>
    lapply(t) |>
    lapply(as.vector) |>
    enframe(name = 'comparison', value = 'transform_diff') |>
    unnest_longer(transform_diff) |>
    mutate(transform_diff = case_when(is.nan(transform_diff) ~ NA,
                                      !is.nan(transform_diff)~transform_diff)
    )
  
  n <- length(unique(y$comparison))
  # add lat_bin, long_bin columns
  y <- y |> mutate(long_bin = rep(rep(1:200, each = 200), n),
                   lat_bin = rep(rep(1:200, times = 200), n),
                   year_mon = as.yearmon(substring(comparison, 1, 8)))|> 
    arrange(year(year_mon), month(year_mon))
  
  # make comparison into a factor, making sure its ordered correctly
  y$comparison <- factor(y$comparison, 
                         levels = unique(y$comparison), 
                         ordered = TRUE)
  return(y)
}

# do month-on-month comparisons within a year for a given set of years
compare_months <- function(data_in, years, smooth_type){
  yms <- unique(data_in$year_mon)
  
  # make a data frame that has transformed differences
  x <- map(yms, ~df_to_mat(data_in, over = .x, nest_by = 'ym')) |> 
    set_names(yms) |> 
    lapply(t) |>
    lapply(as.vector) |> 
    enframe(name = 'year_mon', value = 'obs_freq') |>
    expand(nesting(year_mon = as.yearmon(year_mon),
                   obs_freq),
           nesting(year_mon2 = as.yearmon(year_mon),
                   obs_freq2 = obs_freq)) |>
    filter(year(year_mon) %in% years,
           year(year_mon) == year(year_mon2),
           month(year_mon) == month(year_mon2)-1) |>
    mutate(comparison = paste(year_mon, year_mon2, sep = '_'),
           diff = map2(obs_freq2, obs_freq, `-`)) |>
    select(-obs_freq, -obs_freq2, -year_mon, -year_mon2) |>
    unnest_longer(diff) |>
    mutate(diff = case_when(is.nan(diff) ~ NA,
                            !is.nan(diff) ~ diff),
           transform_diff = case_when(is.na(diff) ~ NA,
                                      diff < 0 ~ -sqrt(abs(diff)),
                                      diff == 0 ~ 0,
                                      diff > 0 ~ sqrt(abs(diff)))
    )
  
  n <- length(unique(x$comparison))
  # add lat_bin, long_bin columns
  x <- x |> mutate(long_bin = rep(rep(1:200, each = 200), n),
                   lat_bin = rep(rep(1:200, times = 200), n))  
  
  com <- unique(x$comparison)
  y <- map(com, ~df_to_mat(x, over = .x, nest_by = 'comparison')) |>
    set_names(com)
  if(smooth_type == 'flat'){
    y <- map(y, flat_smooth)
  }else if(smooth_type == 'geom'){
    y <- map(y, geom_smooth) 
  }else{
    stop('Need valid smooth type, either flat or geom')
  }
  
  y <- set_names(y, com) |>
    lapply(t) |>
    lapply(as.vector) |>
    enframe(name = 'comparison', value = 'transform_diff') |>
    unnest_longer(transform_diff) |>
    mutate(transform_diff = case_when(is.nan(transform_diff) ~ NA,
                                      !is.nan(transform_diff)~transform_diff)
    )
  
  n <- length(unique(y$comparison))
  # add lat_bin, long_bin columns
  y <- y |> mutate(long_bin = rep(rep(1:200, each = 200), n),
                   lat_bin = rep(rep(1:200, times = 200), n),
                   year_mon = as.yearmon(substring(comparison, 1, 8)))|> 
    arrange(year(year_mon), month(year_mon))
  
  # make comparison into a factor, making sure its ordered correctly
  y$comparison <- factor(y$comparison, 
                         levels = unique(y$comparison), 
                         ordered = TRUE)
  return(y)
}

