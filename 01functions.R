# Declares custom functions for later use. Should be run first.

# Geographic Subsampling =======================================================

# ============================== DESCRIPTION ================================ #
# Follow spatial subsampling protocols outlined by Johnston et al (2021)
#> https://github.com/ali-johnston/ebird_sdms_DD_paper/blob/v1.0/R/hex-sample.R
#> Modified code only slightly (ie., commenting for clarity, removing 
#> parts that are unnecessary for my analysis).
#> Removed the regime argument because I will always be sampling detections and
#> non-detections together.
#> This mitigates some spatial bias and reduces file sizes.
# ============================================================================ #

# time.step eg., 'week'. Note that we need to have created a column
#> eg., Week that has the time step we want.

hex_sample <- function(data.set, spacing.km = 5, time.step = NULL) {
  # don't let the function go too far if anything is wrong
  stopifnot(is.data.frame(data.set))
  stopifnot(c("observation_date", "longitude", "latitude", time.step) %in% names(data.set))
  stopifnot(is.numeric(spacing.km), length(spacing.km) == 1, spacing.km > 0)
  
  
  data.set <- data.set |>
    mutate(time.step=as.character(as.vector(as.matrix(data.set[,time.step]))))
  
  
  # generate hexagonal grid
  hex_grid <- dggridR::dgconstruct(spacing = spacing.km)
  
  # get hexagonal cell id and week number for each checklist
  #> takes latitude and longitude and converts to grid cell ID
  #> seqnum is the cell ID
  data_cells <- data.set |> 
    mutate(
      cell_ID = dggridR::dgGEO_to_SEQNUM(hex_grid, 
                                         longitude, 
                                         latitude)$seqnum)
  
  # subsample data by taking 2 observations per grid cell
  data_subsample <- data_cells |>
    dplyr::group_by(time.step, cell_ID) |>
    dplyr::sample_n(size = 2) |> 
    dplyr::ungroup() |>
    dplyr::select(-cell_ID, -time.step) # get rid of extraneous information
  #> now that we don't need it
  return(data_subsample)
}



# Project data for plotting ===================================================

# ============================== DESCRIPTION ================================ #
# Take a dataframe with latitude and longitude columns, create new dataframe
#> with coordinates that are ready for plotting.
#> Modified from Johnston et al., 2021
#> https://github.com/ali-johnston/ebird_sdms_DD_paper/blob/v1.0/03_validation_data.R
# =========================================================================== #

#uses WGS84 projection
project_data <- function(data.set){
  stopifnot(is.data.frame(data.set))
  stopifnot(c('latitude', 'longtitude') %in% names(data.set))

  out <- as.matrix(cbind(data.set$longitude, data.set$latitude)) |>
  sp::SpatialPoints(proj4string = sp::CRS('+init=espg:4326')) |>
  sf::st_as_sf() |>
  sf::st_transform(crs=sf::st_crs(5070))
  
  return(out)
}