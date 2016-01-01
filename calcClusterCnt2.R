calcClusterCnt2 <- function(tconf, visCmpIds, obs)
{
  if (length(visCmpIds) == 0)
  {
    return(list(x = NA, y = NA, t = NA, r = NA))
  }
  
  visCmpObs <- obs[obs$id %in% visCmpIds,] # Visible component observations.
  stopifnot(nrow(visCmpObs[is.na(visCmpObs$x),]) == 0)

  ts <- visCmpObs$t
  refPtInd <- which(ts == max(ts))[1]
  clusterPts <- visCmpObs[, tconf$xyInds]
  clusterWs <- calcClusterCenWeights(tconf, visCmpObs, refPtInd, 1:length(visCmpIds))
  newClusterCen <- calcClusterCnt(clusterPts, clusterWs)
  tstate$objs$x[ind] <- newClusterCen$x
  tstate$objs$y[ind] <- newClusterCen$y
  tstate$objs$t[ind] <- max(ts)
  tstate$objs$r[ind] <- max(tstate$pts$r[ptsInds]) # !!!! Must be changed !!!!
  
  
  clusterPts <- visCmpObs[,tconf$xyInds]
  
  refTime <- max(visCmpObs$t)
  refPtInd <- which(visCmpObs$t == refTime)[1]
  ptsInds <- which(obs$id %in% visCmpObs)
  clusterWs <- w[refPtInd, ptsInds]
  
  clusterCen <- clusterCenCalc(clusterPts, clusterWs)
  return(clusterCen)
}

# calcClusterCnt2 <- function(obs, xyInds, w, cmpIds)
# {
#   visCmpObs <- obs[obs$id %in% cmpIds && !is.na(obs$x),] # Visible component observations.
#   if (nrow(visCmpObs) == 0)
#   {
#     return(list(x = NA, y = NA))
#   }
#   
#   clusterPts <- visCmpObs[,xyInds]
#   
#   refTime <- max(visCmpObs$t)
#   refPtInd <- which(obs$id %in% visCmpObs && obs$t == refTime)[1]
#   ptsInds <- which(obs$id %in% visCmpObs)
#   clusterWs <- w[refPtInd, ptsInds]
#   
#   clusterCen <- clusterCenCalc(clusterPts, clusterWs)
#   return(clusterCen)
# }