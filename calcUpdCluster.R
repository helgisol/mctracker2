calcUpdCluster <- function(
  tconf,
  tstate,
  oldCmpIds, # Old component IDs.
  oldObj) # Old object (cluster center) values.
{
  stopifnot(length(oldCmpIds) > 0)
  
  updCluster <- list(
    cmpIds = oldCmpIds, # Consistent components' IDs.
    ncCmpIds = integer(), # Nonconsistent components' IDs.
    obj = oldObj) # Updated object (cluster center) values.
  
  oldObs <- tstate$obss[[1]] # Old (previous) observations.
  newObs <- tstate$obs # New (current) observations.
  
  newCmpIds <- newObs$id[newObs$id %in% oldCmpIds] # New component IDs.
  
  # Process deleted components.
  isNeedUpdateCnt <- FALSE
  deletedCmpIds <- setdiff(oldCmpIds, newCmpIds)
  if (length(deletedCmpIds) > 0)
  {
    updCluster$cmpIds <- newCmpIds
    
    if (length(newCmpIds) == 0)
    {
      updCluster$obj$x <- NA
      return(updCluster)
    }
    
    isNeedUpdateCnt <- TRUE
  }
  
  stopifnot(length(newCmpIds) > 0)
  
  #oldCmpIds <- newCmpIds # We remove deletedCmpIds from oldCmpIds.
  
  oldCmpObs <- oldObs[oldObs$id %in% newCmpIds,] # Old component observations.
  newCmpObs <- newObs[newObs$id %in% newCmpIds,] # New component observations.
  
  visCmpIds <- oldCmpObs$id[!is.na(oldCmpObs$x)] # Visible component IDs (old non-hidden components).
  obsCmpIds <- newCmpObs$id[!is.na(newCmpObs$x)] # Observed component IDs (new non-hidden components).
  invisCmpIds <- setdiff(newCmpIds, visCmpIds) # Invisible component IDs (old hidden components).
  unobsCmpIds <- setdiff(newCmpIds, obsCmpIds) # Unobserved component IDs (new hidden components).
  
  # Process visible but unobserved component IDs.
  visUnobsCmpIds <- intersect(visCmpIds, unobsCmpIds) # Visible but unobserved component IDs.
  if (length(visUnobsCmpIds) > 0)
  {
    isNeedUpdateCnt <- TRUE # Cluster center is invalid.
  }
  
  # Process visible and observed component IDs.
  visObsCmpIds <- intersect(visCmpIds, obsCmpIds) # Visible and observed component IDs.
  if (length(visObsCmpIds) > 0)
  {
    if (length(visObsCmpIds) > 1)
    {
      if (isNeedUpdateCnt)
      {
        updCluster$obj <- updateClusterObj(tconf, updCluster$obj, visObsCmpIds, oldCmpObs)
        isNeedUpdateCnt <- FALSE
      }
      oldClusterCenPt <- updCluster$obj[,tconf$xyInds]
      oldClusterCenT <- updCluster$obj$t
      oldClusterCenR <- updCluster$obj$r
      newVisObsCmpObs <- newCmpObs[newCmpObs$id %in% visObsCmpIds,] # Visible and observed component observations.
      newClusterPts <- newVisObsCmpObs[,tconf$xyInds]
      newClusterTs <- newVisObsCmpObs$t
      newClusterRs <- newVisObsCmpObs$r
      newClusterTcs <- newVisObsCmpObs$tc
      minTc <- min(newCmpObs$tc)
      newClusterTcW <- calcClusterTcWeights(tconf, newClusterTcs, minTc)
      dists <- calcPointDist(oldClusterCenPt, newClusterPts)
      dists <- dists - oldClusterCenR - (newClusterRs + 
                                           tconf$dRdT * abs(newClusterTs - oldClusterCenT) *
                                           tconf$clusterRFactorLost * newClusterTcW)
      if (max(dists) <= 0.0)
      {
        break
      }
      goodVisObsCmpInds <- which(dists <= 0.0)
      if (length(goodVisObsCmpInds) == 0)
      {
        goodVisObsCmpInds <- order(dists)[1]
      }
      goodVisObsCmpIds <- visObsCmpIds[goodVisObsCmpInds]
      
      badVisObsCmpIds <- setdiff(visObsCmpIds, goodVisObsCmpIds)
      
      visCmpIds <- setdiff(visCmpIds, badVisObsCmpIds)
      obsCmpIds <- setdiff(obsCmpIds, badVisObsCmpIds)
      visObsCmpIds <- setdiff(visObsCmpIds, badVisObsCmpIds)
      
      updCluster$cmpIds <- setdiff(updCluster$cmpIds, badVisObsCmpIds)
      updCluster$ncCmpIds <- c(updCluster$ncCmpIds, badVisObsCmpIds)
    }
    stopifnot(length(visObsCmpIds) > 0)
    updCluster$obj <- updateClusterObj(tconf, updCluster$obj, visObsCmpIds, newCmpObs)
    isNeedUpdateCnt <- FALSE
  }
  
  # Process invisible but observed component IDs.
  invisObsCmpIds <- intersect(invisCmpIds, obsCmpIds) # Invisible but observed component IDs.
  if (length(invisObsCmpIds) > 0)
  {
    if (length(visObsCmpIds) == 0)
    {
      newInvisObsCmpObs <- newCmpObs[newCmpObs$id %in% invisObsCmpIds,]
      
      revisObsCmpId <- invisObsCmpIds[which.min(newInvisObsCmpObs$tc)]
      visObsCmpIds <- revisObsCmpId
      invisObsCmpIds <- setdiff(invisObsCmpIds, revisObsCmpId)
      updCluster$obj <- updateClusterObj(tconf, updCluster$obj, visObsCmpIds, newCmpObs)
      isNeedUpdateCnt <- FALSE
    }
    if (length(invisObsCmpIds) > 0)
    {
      stopifnot(!isNeedUpdateCnt)
      repeat
      {
        oldClusterCenPt <- updCluster$obj[,tconf$xyInds]
        oldClusterCenT <- updCluster$obj$t
        oldClusterCenR <- updCluster$obj$r
        newInvisObsCmpObs <- newCmpObs[newCmpObs$id %in% invisObsCmpIds,] # Invisible but observed component observations.
        newClusterPts <- newInvisObsCmpObs[,tconf$xyInds]
        newClusterTs <- newInvisObsCmpObs$t
        newClusterRs <- newInvisObsCmpObs$r
        newClusterTcs <- newVisObsCmpObs$tc
        minTc <- min(newCmpObs$tc)
        newClusterTcW <- calcClusterTcWeights(tconf, newClusterTcs, minTc)
        dists <- calcPointDist(oldClusterCenPt, newClusterPts)
        dists <- dists - oldClusterCenR - (newClusterRs + 
                                             tconf$dRdT * abs(newClusterTs - oldClusterCenT) *
                                             tconf$clusterRFactorLost * newClusterTcW)
        goodInvisObsCmpInds <- which(dists <= 0.0)
        goodInvisObsCmpIds <- invisObsCmpIds[goodInvisObsCmpInds]
        
        badInvisObsCmpIds <- setdiff(invisObsCmpIds, goodInvisObsCmpIds)
        
        invisCmpIds <- setdiff(invisCmpIds, badInvisObsCmpIds)
        obsCmpIds <- setdiff(obsCmpIds, badInvisObsCmpIds)
        invisObsCmpIds <- setdiff(invisObsCmpIds, badInvisObsCmpIds)
        
        updCluster$cmpIds <- c(updCluster$cmpIds, goodInvisObsCmpIds)
        updCluster$ncCmpIds <- c(updCluster$ncCmpIds, badInvisObsCmpIds)
      }
    }
    updCluster$obj <- updateClusterObj(tconf, updCluster$obj, visObsCmpIds, newCmpObs)
    isNeedUpdateCnt <- FALSE
  }
  
  if (isNeedUpdateCnt)
  {
    updCluster$obj <- updateClusterObj(tconf, updCluster$obj, visObsCmpIds, newCmpObs)
  }
  return(updCluster)
}