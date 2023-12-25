# Plotting Functions ===========================================================

#> DESCRIPTION: saves ggobj input to a pdf file (letter paper in landscape)
#> with file path 'path' and file name 'name.'
#> ncol, nrow, and facets are required and control facetting and layout.

save_pages <- function(ggobj, path, name, ncol, nrow, facets){
  # error catching
  name_catch <- grepl('.pdf', name)
  stopifnot('name must include .pdf' = name_catch)
  
  # function
  all_plots <- lapply(1:ggforce::n_pages(ggobj), function(j){
    p_save <- ggobj+ggforce::facet_wrap_paginate(facets = facets,
                                                 ncol = ncol,
                                                 nrow = nrow,
                                                 page = j)
    return(p_save)
  }
  )
  
  pdf(file.path(path, name), width = 11, height = 8.5)
  lapply(all_plots, print)
  dev.off()
}

#> DESCRIPTION: special version of save_pages that is designed to work with
#> month-on-month comparisons. This breaks the comparisons up by year; ie.,
#> one page has a full set of monthly comparisons for a year and none from any
#> other years. 

save_pages_break <- function(data_in, path, name, ncol, nrow, facets, plot_type = 'map'){
  # error catching
  name_catch <- grepl('.pdf', name)
  stopifnot('name must include .pdf' = name_catch)
  
  type_catch <- grepl('map', plot_type)|grepl('hist', plot_type)
  stopifnot('plot_type must be map or hist' = type_catch)
  
  # function
  years <- unique(year(data_in$year_mon))
  p_save <- list()
  if(plot_type == 'map'){
    for(j in 1:length(years)){
      p_save[[j]] <- filter(data_in, year(year_mon) == years[j]) |>
        ggplot()+
        geom_raster(aes(x = long_bin, y = lat_bin, fill = transform_diff))+
        facet_wrap(facets = facets, ncol = ncol, nrow = nrow)+
        scale_fill_distiller(palette = 'RdBu', 
                             direction = -1, 
                             na.value = '#cccccc')+
        theme_bw()+
        theme(legend.direction = 'horizontal',
              legend.position = 'bottom')
    }
  }else if(plot_type == 'hist'){
    temp <- data_in |>
            mutate(transform_diff = case_when(transform_diff == 0 ~ NA,
                                              .default = transform_diff))
    for(j in 1:length(years)){
      p_save[[j]] <- filter(temp, year(year_mon) == years[j]) |>
        ggplot()+
        geom_histogram(aes(x = transform_diff), bins = 200)+
        facet_wrap(facets = facets, ncol = ncol, nrow = nrow)+
        theme_bw()
    }
  }

  pdf(paste0(path, name), width = 11, height = 8.5)
  lapply(p_save, print)
  dev.off()
}

#> DESCRIPTION: Given a cutoff value, create a map in ggplot where a cell is 
#> black if the observation frequency is ABOVE that value and white if
#> the observation frequency is BELOW the value.

cutoff_plot <- function(data_in, cutoff, title){
  legend_lab <- paste0('OF over ', title)
  p <- data_in |> dplyr::mutate(over = dplyr::case_when(obs_freq < cutoff ~ 'No',
                                                   obs_freq >= cutoff ~ 'Yes'),
                                over = ordered(over, levels = c('No', 'Yes'))) |>
                   ggplot2::ggplot(ggplot2::aes(x = long_bin, y = lat_bin, fill = over))+
                   ggplot2::geom_raster()+
                   ggplot2::theme_bw()+
                   ggplot2::theme(panel.background = ggplot2::element_rect(fill = '#555555'),
                                  legend.key = ggplot2::element_rect(color = "black"))+
                   ggplot2::scale_fill_manual(values = c('white', 'black'))+
                   ggplot2::labs(fill = legend_lab)
  return(p)
}

# Smoothing Functions ==========================================================

#> DESCRIPTION: Smooth a matrix of data. For a cell x, the new "smoothed" value
#> of x is given by taking the weighted average of x (weight given by <w>) and
#> its non-diagonal neighbors within <scope> cells. If x is NA, do a regular average 
#> of the non-diagonal neighbors (otherwise the weighted average will artificially
#> lower the observation frequency).

geom_smooth <- function(matrix_in, w = 0.75, scope = 1){
  N <- length(matrix_in[1,])
  out <- matrix(NA, nrow = N, ncol = N)
  for(j in 1:N){
    for(k in 1:N){
      temp <- matrix_in
      a <- j-scope
      b <- j+scope
      temp[j, k] <- NA
      valid_rows <- a:b
      valid_rows <- valid_rows[a:b >= 1 & a:b <= N]
      
      a <- k-scope
      b <- k+scope
      valid_cols <- a:b
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


#> DESCRIPTION: Smooth a matrix of data. For a cell x, the new "smoothed" value
#> of x is given by taking the average of x and its non-diagonal neighbors within
#> <scope> cells.

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

#> DESCRIPTION: Turn a data frame into a <n> x <n> matrix for easy use in lapply
#> and map functions. <over> is what the lapply/map is done "over", typically
#> the year-month combinations. <nest_by> gives what we should complete the
#> cells by -- that is, should each unique year-month have a full
#> set of cells ('ym'), or should each unique comparison have a full set of cells
#> ('comparison')?

df_to_mat <- function(df, over, nest_by = 'ym', n = 200){
  # error catching
  nest_check <- grepl('ym', nest_by)|grepl('comparison'|nest_by)
  stopifnot('nest_by must be ym or comparison' = nest_check)
  
  # function
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

#> DESCRIPTION: Given an observation frequency data frame, calculate the 
#> difference in observation frequency between the same bin in different 
#> years. Bins are compared between the same month of consecutive years.

compare_years <- function(data_in, smooth_type){
  # error catching
  type_catch <- grepl('flat', smooth_type)|grepl('geom', smooth_type)
  stopifnot('smooth_type must be flat or geom' = type_catch)
  
  # function
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

#> DESCRIPTION: Given an observation frequency data frame and a set of years, 
#> calculate the difference in observation frequency between the same bin in 
#> different years. Bins are compared between consecutive months of the same 
#> year.

compare_months <- function(data_in, years, smooth_type){
  # error catching
  type_catch <- grepl('flat', smooth_type)|grepl('geom', smooth_type)
  stopifnot('smooth_type must be flat or geom' = type_catch)
  
  # function
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

