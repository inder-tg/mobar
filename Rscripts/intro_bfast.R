
# ---

source( paste0( getwd(), "/Rscripts/auxFUN.R" ) )

# ---

pathRData <- paste0( getwd(), "/RData" )

RDataFILES <- list.files( path = pathRData,
                          pattern = ".RData", 
                          full.names = TRUE )

# ---

nbrSpline <- LoadToEnvironment(RDataFILES[1])$nbr
ndvi <- LoadToEnvironment(RDataFILES[2])$ndvi
ndviSpline <- LoadToEnvironment(RDataFILES[3])$ndvi

# ---

toPlot <- list()

toPlot[[1]] <- 655860
toPlot[[2]] <- 2285070

pixel <- get_timeSeries_byClicking(toPlot = toPlot, df=ndvi)

pixel_ts <- ts(as.numeric(pixel$ts), 
               start = c(2003, 1), 
               end = c(2016, 23),
               frequency = 23)

plot(pixel_ts)

# ---

pixel_spline <- get_timeSeries_byClicking(toPlot = toPlot, df=ndviSpline)

pixel_spline_ts <- ts(as.numeric(pixel_spline$ts),
                      start = c(2003, 1),
                      end = c(2016, 23),
                      frequency = 23)

plot(pixel_spline_ts)

ts.plot(pixel_ts, pixel_spline_ts, col = c("blue", "red"), 
        gpars = list(legend=list("topright", col=c("blue", "red"), 
                      legend=c("original", "spline"))))

pixels_df <- data.frame(year=rep(unlist(sapply(2003:2016, function(s) rep(s,23))),
                                  2),
                        type=c(rep("original", length(pixel$ts)), 
                               rep("spline", length(pixel_spline$ts))),
                        value=c(pixel_ts, pixel_spline_ts))


pixels_df <- data.frame(year=rep(seq(2003,2016, length=23*14),
                                 2),
                        type=c(rep("original", length(pixel$ts)), 
                               rep("spline", length(pixel_spline$ts))),
                        value=c(pixel_ts, pixel_spline_ts))



ggplot(pixels_df,    
       aes(x = year,
           y = value,
           col = type)) +
  geom_line()

















