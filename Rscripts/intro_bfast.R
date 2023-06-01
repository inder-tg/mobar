
# ---
library(ggplot2)
library(bfast)

source( paste0( getwd(), "/Rscripts/auxFUN.R" ) )

# ---

pathRData <- paste0( getwd(), "/RData" )

RDataFILES <- list.files( path = pathRData,
                          pattern = ".RData", 
                          full.names = TRUE )

# ---

nbr <- LoadToEnvironment(RDataFILES[1])$nbr_trun
ndvi <- LoadToEnvironment(RDataFILES[2])$ndvi_trun

# ---

toPlot <- list()

toPlot[[1]] <- 655860
toPlot[[2]] <- 2285070

pixel_ndvi <- get_timeSeries_byClicking(toPlot = toPlot, df=ndvi)

pixel_ndvi_ts <- ts(as.numeric(pixel_ndvi$ts), 
               start = c(2003, 1), 
               end = c(2016, 23),
               frequency = 23)

plot(pixel_ndvi_ts, ylab="NDVI", main="NDVI (formato INT2S)")

# ---

pixel_nbr <- get_timeSeries_byClicking(toPlot = toPlot, df=nbr)

pixel_nbr_ts <- ts(as.numeric(pixel_nbr$ts),
                      start = c(2003, 1),
                      end = c(2016, 23),
                      frequency = 23)

plot(pixel_nbr_ts, ylab="NBR", main="NBR (formato INT2S)")

ts.plot(pixel_ndvi_ts, pixel_nbr_ts, col = c("blue", "red")) 
legend("topright", col=c("blue", "red"), 
       lty=rep(1,2), bty="n",
       legend=c("NDVI", "NBR"))

# ---

pixels_df <- data.frame(year=rep(seq(2003, 2016, length=23*14),
                                 2),
                        type=c(rep("NDVI", length(pixel_ndvi$ts)), 
                               rep("NBR", length(pixel_nbr$ts))),
                        value=c(pixel_ndvi_ts, pixel_nbr_ts))

ggplot(pixels_df,    
       aes(x = year,
           y = value,
           col = type)) +
  geom_line()


# ---

pixel_ndvi_bfast <- bfast(pixel_ndvi_ts,
                          # h = 23/322,
                          h = 0.15,
                          # breaks = 5,
                          season = "harmonic")

plot(pixel_ndvi_bfast)

# ---

n_iter <- length(pixel_ndvi_bfast$output)

bps <- pixel_ndvi_bfast$output[[n_iter]]$bp.Vt$breakpoints

bps_years <- getYear(bp=bps, start = 2003, end=2016, freq=23)

pixel_dNBR_dNDVI <- get_dNBR_dNDVI(ndvi=pixel_ndvi$ts, 
                                   nbr=pixel_nbr$ts, 
                                   breaks = bps)

pixel_vegCondition <- vegCondition(x=pixel_dNBR_dNDVI$dNBR)

sev_df <- data.frame(breaks=bps, years=bps_years, 
                     dNBR=pixel_dNBR_dNDVI$dNBR, 
                     vegetationChange=pixel_vegCondition)


sev_df

years <- 2003:2016

as.numeric( years %in% bps_years )

# ---

plot_ndvi_nbr_cps(ndvi=pixel_ndvi, nbr=pixel_nbr, ndvi_bfast=pixel_ndvi_bfast)

  







