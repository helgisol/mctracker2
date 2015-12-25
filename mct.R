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
source('visualizeClusters.R')

#tdata <- list(pts = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double(),R=double(),g=integer()), cmps=list(), objs = data.frame(id=integer(),x=double(),y=double(),t=double()), w=matrix(double(),nrow=0,ncol=0))
#obs <- sampleObs()
#tdata2 <- mct(tdata, obs[[1]])
#tdata3 <- mct(tdata2, obs[[2]])
#visualizeClusters()

updateClusterCnt <- function(tdata, xyInds, ind)
{
  cmps <- tdata$cmps[[ind]]
  cmps <- cmps[!is.na(tdata$pts$x[tdata$pts$id %in% cmps])]
  if (length(cmps) == 0)
  {
    tdata$objs$x[ind] <- NA
    return
  }
  ptsInds <- which(tdata$pts$id %in% cmps)
  ts <- tdata$pts$t[ptsInds]
  refPtInd <- ptsInds[which(ts == max(ts))][1]
  clusterPts <- tdata$pts[ptsInds, xyInds]
  clusterWs <- tdata$w[refPtInd, ptsInds]
  newClusterCen <- clusterCenCalc(clusterPts, clusterWs)
  tdata$objs$x <- newClusterCen$x
  tdata$objs$y <- newClusterCen$y
}

updateExistingClusters <- function(tdata, obs, xyInds)
{
  if (length(tdata$cmps) > 0)
  {
    for (i in 1:length(tdata$cmps))
    {
      cmps <- tdata$cmps[[i]]
      oldPts <- tdata$pts[tdata$pts$id %in% cmps,]
      newPts <- obs[obs$id %in% cmps, ]
      newHiddenPts <- newPts[is.na(newPts$x),]
      if (nrow(newHiddenPts) > 0)
      {
        tdata$pts[tdata$pts$id %in% newHiddenPts$id,] <- newHiddenPts
        updateClusterCnt(tdata, xyInds, i)
      }
    }
  }
}

mct <- function(tdata, obs)
{
  xyInds <- which(colnames(obs) %in% c("x","y")) # Point coordinate indices in observation data frame.
  
  obsNewIds <- setdiff(obs$id, tdata$pts$id) # Vector of ID's for new points.
  obsDelIds <- setdiff(tdata$pts$id, obs$id) # Vector of ID's for deleted points.
  obsExiIds <- setdiff(obs$id, obsNewIds) # Vector of ID's for existed points.
  #obsHidIds <- obsExiIds[is.na(obsExiIds$x)] # Vector of ID's for hidden points.
  
  updateExistingClusters(tdata, obs, xyInds)

  tol <- 1e-3 # Tolerance for mean shift process breaking.
  dRdT <- 4 # Radius growth time factor for time difference correction.
  d <- distMapCalc(obs, dRdT) # Distance map for all points. d[i,i] is filled, but distance for same group's point is NA.
  w <- clusterCenWeightMapCalc(obs, d, dRdT) # Calculate weight map for cluster center calculation.
  n <- nrow(obs) # Number of points.

  clusters <- clusterCalc(obs, d, w, n, xyInds, tol, dRdT)
  components <- lapply(clusters$inds, function(x) obs$id[x])
  objects <- data.frame(
    id = clusters$id,
    x = clusters$x,
    y = clusters$y,
    t = clusters$t);

  newTdata <- list(pts = obs, cmps = components, objs = objects, w = w)
  return(newTdata)
}