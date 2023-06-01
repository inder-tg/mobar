
neededPackages <- c("raster", "mapview", "geoTS", "RColorBrewer",
                    "rasterVis", "ggplot2", "dplyr", "gtools",
                    "bfast")

packagesToInstall <- neededPackages[!(neededPackages %in% installed.packages()[,"Package"])]

if( length(packagesToInstall) ){
  for( i in 1:length(packagesToInstall) ){
    message("Installing package", packagesToInstall[i], "\n")
    install.packages(packagesToInstall[i], dependencies = TRUE)
  }
} 



