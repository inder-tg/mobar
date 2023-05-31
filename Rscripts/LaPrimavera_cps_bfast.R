
# --- Escrito por Inder Tecuapetla, Mayo 11, 2023
# --- Análisis de punto de cambio (bfast)
# --- DATASET (La Primavera): 
# ---   rTp_nbr_spline_trun_CELL_4.RData
# ---   rTp_ndvi_trun_CELL_4.RData

library(RColorBrewer)
library(foreach)
library(doParallel)
library(raster)
library(geoTS)
# library(tmap)
# library(sta)

library(bfast)
library(rasterVis)
library(ggplot2)
library(tidyverse)

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

# --- 

# pixel_bfast <- bfast(pixel_ts, season = "harmonic")
# 
# pixel_bfast$nobp$Vt
# 
# LENGTH <- length(pixel_bfast$output)
# 
# bps <- pixel_bfast$output[[LENGTH]]$bp.Vt$breakpoints
# 
# bps_YEARS <- getYear(end=2009, bp = bps)

years <- 2003:2016

# as.numeric( years %in% bps_YEARS )

# ---

# --- CODIGO EN PARALELO

# YEARS, BPS

# --- en df_YEARS 
# --- en df_BPS

cps_YEARS <- matrix(nrow=nrow(ndvi), ncol=2+length(years))
cps_YEARS[,1:2] <- ndvi[,1:2]

numCores <- detectCores()

# --- progress report file (to check out on the process)

progressReportFile <- paste0(getwd(), "/RData/progressReports/cps_bfast_analysis.txt" )
file.create(path=progressReportFile, showWarnings=FALSE)

write("===BFAST analysis began at===",
      file=progressReportFile, append=TRUE)
write(as.character(Sys.time()[1]), file=progressReportFile,
      append=TRUE)

kluster <- parallel::makeCluster(numCores-1, outfile="")
registerDoParallel(kluster)

# 

output <- foreach(i=1:nrow(ndvi), .combine="rbind",
                  .packages=c("bfast") ) %dopar% { # nrow(sp_ndvi_rTp)
                    
                    pixel_ndvi <- ndvi[i,-c(1,2)]
                    
                    pixel_nbr <- nbr[i,-c(1,2)]
                    
                    pixel_ndvi_ts <- ts(pixel_ndvi, 
                                   start = c(2003,1), 
                                   end = c(2016, 23),
                                   frequency = 23)
                    
                    pixel_bfast <- bfast(pixel_ndvi_ts, season = "harmonic")
                    
                    s <- numeric(length(years))
                    
                    if( !pixel_bfast$nobp$Vt ) {
                      n_iter <- length(pixel_bfast$output)
                      
                      bps <- pixel_bfast$output[[n_iter]]$bp.Vt$breakpoints
                      
                      bps_YEARS <- getYear(start=2003, end=2016, bp = bps)
                      
                      s <- as.numeric( years %in% bps_YEARS )
                    }
                    
                    if(i %% 100 ==0){
                      texto <- paste0("Working on ROW: ", i)
                      write(texto, file=progressReportFile, append=TRUE)
                    }
                    
                    return(s)
                  }
stopCluster(kluster)

write( as.character(Sys.time()[1]), file=progressReportFile, append=T)
write( "===BFAST analysis ended here===", file=progressReportFile, append=T)

# ---

cps_YEARS[,3:12] <- output[,1:10]

save(cps_YEARS, file=paste0(getwd(),"/RData/YEARS_CELL_4.RData"))

# --- RASTERIZACION

# df_YEARS <- LoadToEnvironment( paste0(getwd(),"/RData/mohinora_cps_bfast/YEARS.RData") )$df_YEARS

r <- raster( paste0( getwd(), "/data/mohinora/MOD13Q1_mohinora_mask.tif" ) )

PROJECTION <- raster::projection(r)

# --- 

map_YEARS <- brick()

for(i in 3:ncol(df_YEARS)){
  TEMP <- matrixToRaster(matrix=df_YEARS[,c(1:2,i)], projection=PROJECTION)
  
  map_YEARS <- addLayer(map_YEARS, TEMP)
}

map_YEARS

# --- visualization

# --- just the ANP Cerro Mohinora

SHP_anp <- list.files( path = paste0( getwd(), "/data/anp_2021" ),
                       pattern = ".shp", 
                       full.names = TRUE )

shp_anp <- shapefile( SHP_anp[1] )

shp_anp_sinu <- spTransform(shp_anp, crs(r))

map_YEARS_mohinora <- raster_intersect_sp(map_YEARS, shp_anp_sinu)

levelplot(map_YEARS_mohinora,
          main="Abrupt changes",
          names.attr=as.character(2000:2009))

# ---

mohinora_rTp <- rasterToPoints(subset(map_YEARS_mohinora,1))

COUNTS <- c()
for(i in 1:nlayers(map_YEARS_mohinora)){
  values <- getValues(subset(map_YEARS_mohinora,i))
  values <- values[!is.na(values)]
  COUNTS <- c(COUNTS, sum(values == 1))
}

CHANGES_DATA <- data.frame(YEARS = as.character(2000:2009),
                           PERCENT = COUNTS / nrow(mohinora_rTp) * 1e2)

COLORES <- c(rev(brewer.pal(10, "Set3")))

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








