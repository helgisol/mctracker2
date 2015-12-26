calcClusterCnt <- function(obs, xyInds, w, cmpIds)
{
  visCmpObs <- obs[obs$id %in% cmpIds && !is.na(obs$x),] # Visible component observations.
  if (nrow(visCmpObs) == 0)
  {
    return(list(x = NA, y = NA))
  }

  clusterPts <- visCmpObs[,xyInds]
  
  refTime <- max(visCmpObs$t)
  refPtInd <- which(obs$id %in% visCmpObs && obs$t == refTime)[1]
  ptsInds <- which(obs$id %in% visCmpObs)
  clusterWs <- w[refPtInd, ptsInds]
  
  clusterCen <- clusterCenCalc(clusterPts, clusterWs)
  return(clusterCen)
}