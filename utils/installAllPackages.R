

# Get a list of installed packages
installedPackages <- installed.packages()

# Let's start with INLA and intSDM
if(!("INLA" %in% row.names(installedPackages))) {
  install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
  }
if(!("intSDM" %in% row.names(installedPackages))) {
  devtools::install_github("PhilipMostert/intSDM") 
}

# Now we bring int he rest of the necessary packages
necessaryPackages <- c("rgbif", "sf", "stringr", "dplyr", "rinat", "raster", "csmaps", "ggplot2",
                       "shiny", "shinydashboard", "PointedSDMs", "terra",
                       "shinyjs", "inlabru", "randomcoloR", "plotKML")
uninstalledPackages <- necessaryPackages[!(necessaryPackages %in% row.names(installedPackages))]
install.packages(uninstalledPackages)
