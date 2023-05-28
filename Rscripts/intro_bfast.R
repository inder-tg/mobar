
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

plot(pixel_ndvi_ts)

# ---

pixel_nbr <- get_timeSeries_byClicking(toPlot = toPlot, df=nbr)

pixel_nbr_ts <- ts(as.numeric(pixel_nbr$ts),
                      start = c(2003, 1),
                      end = c(2016, 23),
                      frequency = 23)

plot(pixel_nbr_ts)

ts.plot(pixel_ndvi_ts, pixel_nbr_ts, col = c("blue", "red")) 
legend("topright", col=c("blue", "red"), 
       lty=rep(1,2),
       legend=c("NDVI", "NBR"))

# ---

# pixels_df <- data.frame(year=rep(unlist(sapply(2003:2016, function(s) rep(s,23))),
#                                   2),
#                         type=c(rep("NDVI", length(pixel_ndvi$ts)), 
#                                rep("NBR", length(pixel_nbr$ts))),
#                         value=c(pixel_ndvi_ts, pixel_nbr_ts))


pixels_df <- data.frame(year=rep(seq(2003,2016, length=23*14),
                                 2),
                        type=c(rep("NDVI", length(pixel_ndvi$ts)), 
                               rep("NBR", length(pixel_nbr$ts))),
                        value=c(pixel_ndvi_ts, pixel_nbr_ts))



ggplot(pixels_df,    
       aes(x = year,
           y = value,
           col = type)) +
  geom_line()

















