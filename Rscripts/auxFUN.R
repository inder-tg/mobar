

# --- 

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  return(env) 
}

# ---

get_timeSeries_byClicking <- function(toPlot, df){
  
  # toPlot=toPlot; df=ndvi
  
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