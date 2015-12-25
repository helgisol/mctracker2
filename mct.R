source('sampleObs.R')
source('distMapCalc.R')
source('clusterCenWeightMapCalc.R')
source('distCalc.R')
source('initSeedClustersCalc.R')
source('seedClusterCalc.R')
source('clusterCenCalc.R')
source('conflictingRankCalc.R')
source('isSeedClusterDetachableCalc.R')
source('detachPoint.R')
source('clusterCalc.R')

tdata <- list(pts = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double(),R=double(),g=integer()), cmps=list(), objs = data.frame(id=integer(),x=double(),y=double(),t=double()))
obs <- sampleObs()
tdata2 <- mct(tdata, obs[[1]])
#tdata3 <- mct(tdata2, obs[[2]])
visualizeClusters()

visualizeClusters <- function(tdata = tdata2)
{
  #library(ggplot2)
  library("plotrix")
  #qplot(x, y, data = obs[[1]], color = as.factor(g))
  plot(tdata$pts$x, tdata$pts$y, col=tdata$pts$g, pch=20, asp=1, xlab="x", ylab="y")
  text(tdata$pts$x, tdata$pts$y, as.character(tdata$pts$id), pos=4, col=tdata$pts$g)
  points(tdata$objs$x, tdata$objs$y, pch=23, col=max(tdata$pts$g)+1)
  text(tdata$objs$x, tdata$objs$y, as.character(tdata$objs$id), pos=2, col=max(tdata$pts$g)+1)
  draw.circle(tdata$objs$x, tdata$objs$y, tdata$pts$r[1], border=max(tdata$pts$g)+1)
  for(i in 1:length(tdata$cmps))
  { 
    xCen <- tdata$objs$x[i]
    yCen <- tdata$objs$y[i] 
    cmp <- tdata$cmps[[i]]
    for(id in cmp)
    {
      xPt <- tdata$pts$x[id]
      yPt <- tdata$pts$y[id]
      points(c(xCen, xPt), c(yCen, yPt), col=tdata$pts$g[id], type="o")
    }
  }
}


mct <- function(tdata, obs)
{
  obsNewIds <- setdiff(obs$id, tdata$pts$id) # Vector of ID's for new points.
  obsExiIds <- setdiff(tdata$pts$id, obsNewIds) # Vector of ID's for existed points.
  
  
  

  tol <- 1e-3 # Tolerance for mean shift process breaking.
  dRdT <- 4 # Radius growth time factor for time difference correction.
  d <- distMapCalc(obs, dRdT) # Distance map for all points. d[i,i] is filled, but distance for same group's point is NA.
  w <- clusterCenWeightMapCalc(obs, d, dRdT) # Calculate weight map for cluster center calculation.
  n <- nrow(obs) # Number of points.
  xyInds <- which(colnames(obs) %in% c("x","y")) # Point coordinate indices in observation data frame.

  clusters <- clusterCalc(obs, d, w, n, xyInds, tol, dRdT)
  components <- clusters$inds
  objects <- data.frame(
    id = clusters$id,
    x = clusters$x,
    y = clusters$y,
    t = clusters$t);

  newTdata <- list(pts = obs, cmps = components, objs = objects)
  return(newTdata)
}