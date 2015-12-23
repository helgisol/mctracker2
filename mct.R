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

visualizeClusters <- function()
{
  #library(ggplot2)
  library("plotrix")
  #qplot(x, y, data = obs[[1]], color = as.factor(g))
  plot(obs[[1]]$x, obs[[1]]$y, col=obs[[1]]$g, pch=20)
  points(tdata2$objs$x, tdata2$objs$y, pch=22)
  #draw.circle(obs[[1]]$x, obs[[1]]$y, obs[[1]]$r)
  draw.circle(tdata2$objs$x, tdata2$objs$y, obs[[1]]$r[1])
}


mct <- function(tdata, obs)
{
  obsNewIds <- setdiff(obs$id, tdata$pts$id) # Vector of ID's for new points.
  obsExiIds <- setdiff(tdata$pts$id, obsNewIds) # Vector of ID's for existed points.
  
  
  

  d <- distMapCalc(obs) # Distance map for all points. d[i,i] is filled, but distance for same group's point is NA.
  w <- clusterCenWeightMapCalc(obs, d) # Calculate weight map for cluster center calculation.
  n <- nrow(obs) # Number of points.
  xyInds <- which(colnames(obs) %in% c("x","y")) # Point coordinate indices in observation data frame.
  tol <- 1e-3 # Tolerance for mean shift process breaking.

  clusters <- clusterCalc(obs, d, w, n, xyInds, tol)
  components <- clusters$inds
  objects <- data.frame(
    id = clusters$id,
    x = clusters$x,
    y = clusters$y,
    t = clusters$t);

  newTdata <- list(pts = obs, cmps = components, objs = objects)
  return(newTdata)
}