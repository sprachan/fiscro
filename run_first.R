# Functions ----
manage_packages <- function(packages){
  check_req <- lapply(packages, \(x) system.file(package = x)) |>
    lapply(nchar) |>
    unlist() 
  need <- unlist(packages)[ifelse(check_req > 0, FALSE, TRUE)]
  if(length(need) == 0){
    cat('All required packages are installed. You can close this and proceed with other scripts.')
  }else{
    cat('Missing Package(s): ', need, '\n')
    y <- menu(c('Yes', 'No'),
              title = 'Do you want to install missing packages?')
    if(y == 1){
      install.packages(need)
      cat('All required packages are installed. You can close this and proceed with other scripts.')
    }else{
      cat('These packages are required for these scripts to work, so expect errors from missing packages.')
    }
  }
}

# Describe and Optionally Check Package Dependencies ----
cat('Package Dependencies (must be installed): ', '\n',
    'Tidyverse packages: ggplot2, dplyr, lubridate, purrr, tidyr', '\n',
    'Additional plot package: ggforce', '\n',
    'Command-line Integration packages: optparse, arrow', '\n',
    'Spatial Data packages: terra, prism', '\n')

manage_packages(as.list(c('ggplot2', 'dplyr', 'lubridate', 'purrr', 'tidyr',
                          'ggforce', 'optparse', 'arrow', 'terra', 'prism')))
