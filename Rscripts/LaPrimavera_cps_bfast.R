
# --- Escrito por Inder Tecuapetla, Mayo 11, 2023
# --- Análisis de punto de cambio (bfast)
# --- DATASET (La Primavera): 
# ---   rTp_nbr_spline_trun_CELL_4.RData
# ---   rTp_ndvi_trun_CELL_4.RData

library(RColorBrewer)
library(raster)
library(geoTS)
library(rasterVis)
library(ggplot2)
library(dplyr)
library(mapview)
library(gtools)

# --- Descomente las 3 líneas de abajo si desea aplaicar
# --- este análisis al DATASET (La Primavera) desde su sistema
# library(bfast)
# library(foreach)
# library(doParallel)
# ---------------------------------------------------------------

source(paste0(getwd(), "/Rscripts/auxFUN.R"))

# ---

pathRData <- paste0( getwd(), "/RData" )

RDataFILES <- list.files( path = pathRData,
                          pattern = ".RData", 
                          full.names = TRUE )

# ---

nbr <- LoadToEnvironment(RDataFILES[1])$nbr_trun
ndvi <- LoadToEnvironment(RDataFILES[2])$ndvi_trun

# --- 

years <- 2003:2016

# ---

# --- CODIGO EN PARALELO

# YEARS

# --- en cps_YEARS guardaremos 0 (sin punto de cambio) o 1's (punto de cambio) 
# --- dependiendo si se han detectado puntos de cambio en los 14 años de estudio 

# cps_YEARS <- matrix(nrow=nrow(ndvi), ncol=2+length(years))
# cps_YEARS[,1:2] <- ndvi[,1:2]

# cps_dNBR <- matrix(nrow=nrow(ndvi), ncol=2+length(years))
# cps_dNBR[,1:2] <- nbr[,1:2]
# 
# numCores <- detectCores()
# 
# # --- progress report file (to check out on the process)
# 
# progressReportFile <- paste0(getwd(), "/RData/progressReports/cps_bfast_analysis.txt" )
# file.create(path=progressReportFile, showWarnings=FALSE)
# 
# write("===BFAST analysis began at===",
#       file=progressReportFile, append=TRUE)
# write(as.character(Sys.time()[1]), file=progressReportFile,
#       append=TRUE)
# 
# kluster <- parallel::makeCluster(numCores-1, outfile="")
# registerDoParallel(kluster)
# 
# output <- foreach(i=1:nrow(ndvi), .combine="rbind",
#                   .packages=c("bfast") ) %dopar% { # nrow(sp_ndvi_rTp)
#                     
#                     pixel_ndvi <- ndvi[i,-c(1,2)]
#                     
#                     pixel_nbr <- nbr[i,-c(1,2)]
#                     
#                     pixel_ndvi_ts <- ts(pixel_ndvi, 
#                                    start = c(2003,1), 
#                                    end = c(2016, 23),
#                                    frequency = 23)
#                     
#                     pixel_bfast <- bfast(pixel_ndvi_ts, season = "harmonic")
#                     
#                     s <- numeric(length(years))
#                     
#                     if( !pixel_bfast$nobp$Vt ) {
#                       n_iter <- length(pixel_bfast$output)
#                       
#                       bps <- pixel_bfast$output[[n_iter]]$bp.Vt$breakpoints
#                       
#                       bps_YEARS <- getYear(start=2003, end=2016, bp = bps)
# 
#                       pixel_dNBR_dNDVI <- get_dNBR_dNDVI(ndvi = pixel_ndvi,
#                                                          nbr = pixel_nbr,
#                                                          breaks = bps)
#                         
#                       cpsNDVI_where <- as.numeric( years %in% bps_YEARS )
#                       dNBR_where <- numeric(length(years))
#                       dNBR_where[(1:14)[which(cpsNDVI_where == 1)]] <- pixel_dNBR_dNDVI$dNBR
#                       
#                       s <- c(cpsNDVI_where, dNBR_where)
#                     }
#                     
#                     if(i %% 100 ==0){
#                       texto <- paste0("Working on ROW: ", i)
#                       write(texto, file=progressReportFile, append=TRUE)
#                     }
#                     
#                     return(s)
#                   }
# stopCluster(kluster)
# 
# write( as.character(Sys.time()[1]), file=progressReportFile, append=T)
# write( "===BFAST analysis ended here===", file=progressReportFile, append=T)
# 
# # ---
# 
# cps_YEARS[,3:16] <- output[,1:14]
# cps_dNBR[,3:16] <- output[,1:14+14]
# 
# save(cps_YEARS, file=paste0(getwd(),"/RData/YEARS_CELL_4.RData"))
# save(cps_dNBR, file=paste0(getwd(),"/RData/dNBR_CELL_4.RData"))

# --- RASTERIZACION

# --- La línea de abajo puede omitirse si se ha hecho el análisis en su sistema,
# --- i.e. si el objeto cps_YEARS ya existe en su sesión de R
cps_YEARS <- LoadToEnvironment( paste0(getwd(),"/RData/YEARS_CELL_4.RData") )$cps_YEARS

DIRS <- dir( path = paste0( getwd(), "/data" ),
            full.names = TRUE )

tifFILES <- list.files(path = DIRS[1],
                       pattern = ".tif",
                       full.names = TRUE)

r <- raster( tifFILES[1] )

PROJECTION <- raster::projection(r)

# --- 

map_YEARS <- brick()

for(i in 3:ncol(cps_YEARS)){
  TEMP <- matrixToRaster(matrix=cps_YEARS[,c(1:2,i)], 
                         projection=PROJECTION)
  
  map_YEARS <- addLayer(map_YEARS, TEMP)
}

map_YEARS

# --- visualization

levelplot(map_YEARS,
          main="Abrupt changes",
          names.attr=as.character(2003:2016))

# ---

LaPrimavera_rTp <- rasterToPoints(subset(map_YEARS,1))

COUNTS <- c()
for(i in 1:nlayers(map_YEARS)){
  values <- getValues(subset(map_YEARS,i))
  values <- values[!is.na(values)]
  COUNTS <- c(COUNTS, sum(values == 1))
}

CHANGES_DATA <- data.frame(YEARS = as.character(2003:2016),
                           PERCENT = COUNTS / nrow(LaPrimavera_rTp) * 1e2)

COLORES <- c(rep("black",2), rev(brewer.pal(10, "Set3")), rep("black",2))

CHANGES_DATA %>%
  ggplot( aes(x=YEARS, y=PERCENT) ) +
  geom_segment( aes(x=YEARS, xend=YEARS, 
                    y=0, yend=PERCENT), 
                color="gray", linewidth=2) +
  geom_point(size=5, color=COLORES) +
  coord_flip() +
  theme_classic() +
  theme(legend.title = element_text(face="bold"),
        legend.text = element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.y = element_text(size=24, face="bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none") +
  xlab("") + ylab("%")

# --- Visualization SEVERITY map as a leaftlet

# --- La línea de abajo puede omitirse si se ha hecho el análisis en su sistema,
# --- i.e. si el objeto cps_dNBR ya existe en su sesión de R
cps_dNBR <- LoadToEnvironment( paste0(getwd(),"/RData/dNBR_CELL_4.RData") )$cps_dNBR


shp_FILES <- list.files(path = DIRS[3],
                        pattern = ".shp",
                        full.names = TRUE)

usv_SHP_LP <- shapefile( shp_FILES[1] )

usv_SHP_LP_utm <- spTransform(x = usv_SHP_LP,
                              CRSobj = crs(r))

for(i in 3:ncol(cps_dNBR)){
  
  TEMP <- matrixToRaster(matrix=cps_dNBR[,c(1:2,i)], 
                         projection=PROJECTION)
  
  TEMP_LP <- raster_intersect_sp(x=TEMP, y=usv_SHP_LP_utm)
  
  writeRaster(TEMP_LP, 
              filename = paste0( getwd(), "/TIF/vegCond_map_", years[i-2] ),
              format = "GTiff",
              datatype = "FLT4S",
              overwrite = TRUE)
}


tifFILES <- mixedsort(list.files(path = paste0( getwd(), "/TIF" ),
                     pattern = ".tif",
                     full.names = TRUE))

xmlFILES <- mixedsort(list.files(path = paste0( getwd(), "/TIF" ),
                                 pattern = ".xml",
                                 full.names = TRUE))


sevFILES <- tifFILES[-(1:length(tifFILES))[tifFILES %in% xmlFILES]]

# ---

mapsev2005 <- get_SEVmap(path=sevFILES[3], nameLayer = as.character(years[3]) )
mapsev2006 <- get_SEVmap(path=sevFILES[4], nameLayer = as.character(years[4]) )
mapsev2007 <- get_SEVmap(path=sevFILES[5], nameLayer = as.character(years[5]) )
mapsev2008 <- get_SEVmap(path=sevFILES[6], nameLayer = as.character(years[6]) )
mapsev2009 <- get_SEVmap(path=sevFILES[7], nameLayer = as.character(years[7]) )
mapsev2010 <- get_SEVmap(path=sevFILES[8], nameLayer = as.character(years[8]) )
mapsev2011 <- get_SEVmap(path=sevFILES[8], nameLayer = as.character(years[9]) )
mapsev2012 <- get_SEVmap(path=sevFILES[8], nameLayer = as.character(years[10]) )
mapsev2013 <- get_SEVmap(path=sevFILES[8], nameLayer = as.character(years[11]) )
mapsev2014 <- get_SEVmap(path=sevFILES[8], nameLayer = as.character(years[12]) )

mapsev2005

mapsev2008

mapsev2012











