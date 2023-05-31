

# --- 

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env) 
}

# ---

get_timeSeries_byClicking <- function(toPlot, df){
  
  nRow <- length(unlist(toPlot)) / 2
  
  mat_toPlot <- matrix(as.numeric(unlist(toPlot)), nrow = nRow)
  
  dX <- matrix(NA, nrow = nrow(df))
  
  dY <- matrix(NA, nrow = nrow(df))
  
  aproxX <- numeric(nRow)
  
  aproxY <- numeric(nRow)
  
  dX <- sapply(1:nRow, function(s) abs(df[,1] - mat_toPlot[s,1]))
  
  aproxX <- sapply(1:nRow, function(s) df[which.min(dX[,s]),1] )
  
  dY <- sapply(1:nRow, function(s) abs(df[,2] - mat_toPlot[s,2]))
  
  aproxY <- sapply(1:nRow, function(s) df[which.min(dY[,s]),2] )
  
  toExtract <- matrix(NA, nrow = nRow, ncol = 2)
  
  toExtract[,1] <- aproxX
  toExtract[,2] <- aproxY
  
  pixels <- matrix(NA, nrow = nRow, ncol = ncol(df)-2)
  
  for(i in 1:nRow){
    pixels[i,] <- df[(df[,1] == toExtract[i,1]) & (df[,2] == toExtract[i,2])][-c(1:2)]
  }
  
  list(ts = pixels, coord = toExtract) 
}

# ---

ts_at_breaks <- function(ts, at){
  sapply(1:length(at), function(s) ts[at[s]] )
}

getYear <- function(start=2003, end=2016, bp, freq=23){
  period <- start:end
  totalDays <- c(0, freq * 1:length(start:end))
  
  if( length(bp) == 1 ){
    year <- period[sum( totalDays - bp < 0 )]
  } else {
    year <- unlist( lapply(1:length(bp), function(s) period[sum( totalDays - bp[s] < 0 )]  ) )
  }
  
  year
}

# ---

get_dNBR_dNDVI <- function(ndvi, nbr, breaks, before = 23,
                            after = 1, scale = TRUE, scaleFactor = 1e-4){
  
  validIndices <- FALSE
  validBreaks <- NULL
  dNBR <- NA
  dNDVI <- NA
  
  if(length(breaks) != 0){
    
    validBreaks <- breaks
    
    validIndices <- TRUE
    
    nbrPrevious <- ts_at_breaks(ts=nbr, at=breaks-before) 
    nbrPosterior <- ts_at_breaks(ts=nbr, at=breaks+after) 
    ndviPrevious <- ts_at_breaks(ts=ndvi, at=breaks-before)
    ndviPosterior <- ts_at_breaks(ts=ndvi, at=breaks+after)
    
    dNBR <- nbrPrevious - nbrPosterior
    dNDVI <- ndviPrevious - nbrPosterior
    
    if(scale){
      dNBR <- dNBR * scaleFactor
      dNDVI <- dNDVI * scaleFactor
    }
    
  }
  

list(dNBR=dNBR, dNDVI=dNDVI, validIndice=validIndices, validBreaks=breaks)
  
}

# ---

plot_ndvi_nbr_cps <- function(ndvi, nbr, ndvi_bfast){
  n_iter <- length(ndvi_bfast$output)
  
  AUX <- as.matrix(ndvi_bfast$output[[n_iter]]$ci.Vt$confint)
  
  draw_params <- c(5.1, 4.5, 0.5, 2.1)
  par(mfrow=c(2,1), cex.lab = 1.5, cex.axis = 1.25, 
      mar = draw_params)
  
  plot(as.numeric(ndvi$ts), col = "gray", ylab = "NDVI", xlab = "Years",
       lwd = 4, type = "l", axes=F)
  axis(1, at = seq(1,345, by=46), labels = seq(2003,2017, by=2))
  lines(1:length(ndvi$ts), as.numeric(ndvi_bfast$output[[n_iter]]$Tt), 
        col = "royalblue", lwd = 4)
  yRan <- range(ndvi$ts)
  yAxes <- seq(yRan[1], yRan[2], length=5)
  axis(2, at = yAxes, labels = round(yAxes, 2))
  TEMP <- unlist(sapply(1:nrow(AUX), function(s) AUX[s,1]:AUX[s,3]))
  for(i in 1:nrow(AUX)){
    axis(1, at = c(AUX[i,1], AUX[i,3]), labels = c("", ""), col = "red",
         lwd = 3, lwd.ticks = 4)
  }
  
  plot(as.numeric(nbr$ts), col = "gray", ylab = "NBR", xlab = "Years",
       lwd = 4, type = "l", axes=F)
  axis(1, at = seq(1,345,by=46), labels = seq(2003,2017,by=2))
  points(AUX[,2], nbr$ts[AUX[,2]], pch = 8, lwd = 3)
  for(i in 1:nrow(AUX)){
    points(AUX[i,2]-23, nbr$ts[AUX[i,2]-23], pch = 24, col = "blue", lwd = 3)
    points(AUX[i,2]+1, nbr$ts[AUX[i,2]+1], pch = 25, col = "blue", lwd = 3)
  }
  legend("bottomleft", pch = c(24,8,25), pt.cex = rep(2,3),
         col = c("blue","black","blue"), pt.lwd = c(2,2,2),
         legend = c("Before", "Change", "After"),
         # horiz = TRUE, 
         bty = "n")
  yRan <- range(nbr$ts)
  yAxes <- seq(yRan[1], yRan[2], length=5)
  axis(2, at = yAxes, labels = round(yAxes, 2))
  
}

# ---

vegCondition <- function(x){
  ifelse(x < -0.25, "High-regrowth", 
         ifelse(x < -0.1, "Low-regrowth", 
                ifelse(x < 0.1, "No burned",
                       ifelse(x < 0.27, "Low-severity",
                              ifelse(x < 0.66, "Moderate-severity", "High-severity")))))
  
}
