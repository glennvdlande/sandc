checkPackages <- function(pkg){
  # check if package is installed. If not -> install, afterwards add to current library
  # Input:
  #   pkg = strng array of packages that need to be checked and added (e.g. 
  #         c('tidyverse', 'lubridate))
  # Output:
  #   Packages will be installed and added to library
  #
  # Use: checkPackages(pkg)
  
  # check which packages are not yet installed
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  # if there are any, install these and the dependencies
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  
  # afterwards add all to library
  invisible(sapply(pkg, library, character.only = TRUE))
}