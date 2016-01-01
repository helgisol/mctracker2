updateClusterObj <- function(tconf, obj, visCmpIds, obs)
{
  if (length(visCmpIds) == 0)
  {
    obj$x <- NA
    return(obj)
  }
  
  visCmpObs <- obs[obs$id %in% visCmpIds,] # Visible component observations.
  stopifnot(nrow(visCmpObs[is.na(visCmpObs$x),]) == 0)

  ts <- visCmpObs$t
  tMax <- max(ts)
  refPtInd <- which(ts == tMax)[1]
  clusterPts <- visCmpObs[, tconf$xyInds]
  clusterWs <- calcClusterCenWeights(tconf, visCmpObs, refPtInd, 1:length(visCmpIds))
  newClusterCen <- calcClusterCnt(clusterPts, clusterWs)
  obj$x[ind] <- newClusterCen$x
  obj$y[ind] <- newClusterCen$y
  obj$t[ind] <- tMax
  obj$r[ind] <- max(visCmpObs$r[refPtInd], weighted.mean(visCmpObs$r, clusterWs))
  return(obj)
}