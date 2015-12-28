updateExistingClusters <- function(tconf, oldTstate, obs)
{
  updateExistingCluster <- function(oldTstate, i)
  {
    if (i == 2)
    {
      q1 <- 1
    }
    nonconsistentCmpIds = integer()
    
    oldCmpIds <- oldTstate$cmps[[i]] # Old component IDs.
    newCmpIds <- obs$id[obs$id %in% oldCmpIds] # New component IDs.
    
    oldCmpObs <- oldTstate$pts[oldTstate$pts$id %in% oldCmpIds,] # Old component observations.
    newCmpObs <- obs[obs$id %in% oldCmpIds,] # New component observations.

    visCmpIds <- oldCmpObs$id[!is.na(oldCmpObs$x)] # Visible component IDs (old non-hidden components).
    obsCmpIds <- newCmpObs$id[!is.na(newCmpObs$x)] # Observed component IDs (new non-hidden components).
    invisCmpIds <- setdiff(oldCmpIds, visCmpIds) # Invisible component IDs (old hidden components).
    unobsCmpIds <- setdiff(newCmpIds, obsCmpIds) # Unobserved component IDs (new hidden components).
    
    # Process deleted components.
    isNeedUpdateCnt <- FALSE
    deletedCmpIds <- setdiff(oldCmpIds, newCmpIds)
    if (length(deletedCmpIds) > 0)
    {
      oldTstate$cmps[[i]] <- newCmpIds
      isNeedUpdateCnt <- TRUE
    }
    
    # Process visible but unobserved component IDs.
    visUnobsCmpIds <- intersect(visCmpIds, unobsCmpIds) # Visible but unobserved component IDs.
    if (length(visUnobsCmpIds) > 0)
    {
      oldTstate$pts$x[oldTstate$pts$id %in% visUnobsCmpIds] <- NA
      isNeedUpdateCnt <- TRUE
    }
    
    # Process visible and observed component IDs.
    visObsCmpIds <- intersect(visCmpIds, obsCmpIds) # Visible and observed component IDs.
    if (length(visObsCmpIds) > 0)
    {
      if (length(visObsCmpIds) > 1)
      {
        if (isNeedUpdateCnt)
        {
          oldTstate <- updateClusterCnt(tconf, oldTstate, i)
          isNeedUpdateCnt <- FALSE
        }
        repeat
        {
          #newClusterCen <- calcClusterCnt2(obs, tconf$xyInds, oldTstate$w, cmpIds)
          oldClusterCenPt <- oldTstate$objs[i,tconf$xyInds]
          oldClusterCenT <- oldTstate$objs$t[i]
          oldClusterCenR <- oldTstate$objs$r[i] * tconf$leaveClusterRFactor
          
          newVisObsCmpObs <- newCmpObs[newCmpObs$id %in% visObsCmpIds,] # Visible and observed component observations.
          newClusterPts <- newVisObsCmpObs[,tconf$xyInds]
          newClusterTs <- newVisObsCmpObs$t
          dists <- calcPointDist(oldClusterCenPt, newClusterPts)
          dists <- dists - oldClusterCenR - (newVisObsCmpObs$r + tconf$dRdT * abs(newClusterTs - oldClusterCenT))
          distOrder <- order(dists, decreasing = TRUE)
          if (dists[distOrder[1]] <= 0.0)
          {
            break
          }
          nonconsistentCmpId <- visObsCmpIds[distOrder[1]]
          visCmpIds <- setdiff(visCmpIds, nonconsistentCmpId)
          obsCmpIds <- setdiff(obsCmpIds, nonconsistentCmpId)
          visObsCmpIds <- setdiff(visObsCmpIds, nonconsistentCmpId)
          oldCmpIds <- setdiff(oldCmpIds, nonconsistentCmpId)
          oldTstate$cmps[[i]] <- oldCmpIds
          nonconsistentCmpIds <- c(nonconsistentCmpIds, nonconsistentCmpId)
          oldTstate <- updateClusterCnt(tconf, oldTstate, i)
          if (length(visObsCmpIds) == 1)
          {
            break
          }
        }
      }
      oldTstate$pts[oldTstate$pts$id %in% visObsCmpIds,] <- newCmpObs[newCmpObs$id %in% visObsCmpIds,]
      oldTstate <- updateClusterCnt(tconf, oldTstate, i)
      isNeedUpdateCnt <- FALSE
    }
    
    # Process invisible but observed component IDs.
    invisObsCmpIds <- intersect(invisCmpIds, obsCmpIds) # Invisible but observed component IDs.
    if (length(invisObsCmpIds) > 0)
    {
      oldTstate$pts[oldTstate$pts$id %in% invisObsCmpIds,] <- newCmpObs[newCmpObs$id %in% invisObsCmpIds,]
      if (length(visObsCmpIds) == 0)
      {
        revisObsCmpId <- min(invisObsCmpIds)
        visObsCmpIds <- revisObsCmpId
        invisObsCmpIds <- setdiff(invisObsCmpIds, revisObsCmpId)
        oldTstate <- updateClusterCnt(tconf, oldTstate, i)
        isNeedUpdateCnt <- FALSE
      }
      if (length(invisObsCmpIds) > 0)
      {
        stopifnot(!isNeedUpdateCnt)
        repeat
        {
          oldClusterCenPt <- oldTstate$objs[i,tconf$xyInds]
          oldClusterCenT <- oldTstate$objs$t[i]
          oldClusterCenR <- oldTstate$objs$r[i] * tconf$leaveClusterRFactor
          
          invisObsCmpObs <- newCmpObs[newCmpObs$id %in% invisObsCmpIds,] # Invisible but observed component observations.
          newClusterPts <- invisObsCmpObs[,tconf$xyInds]
          newClusterTs <- invisObsCmpObs$t
          dists <- calcPointDist(oldClusterCenPt, newClusterPts)
          dists <- dists - oldClusterCenR - (invisObsCmpObs$r + tconf$dRdT * abs(newClusterTs - oldClusterCenT))
          distOrder <- order(dists, decreasing = TRUE)
          if (dists[distOrder[1]] <= 0.0)
          {
            break
          }
          nonconsistentCmpId <- invisObsCmpIds[distOrder[1]]
          invisCmpIds <- setdiff(invisCmpIds, nonconsistentCmpId)
          obsCmpIds <- setdiff(obsCmpIds, nonconsistentCmpId)
          invisObsCmpIds <- setdiff(invisObsCmpIds, nonconsistentCmpId)
          oldCmpIds <- setdiff(oldCmpIds, nonconsistentCmpId)
          oldTstate$cmps[[i]] <- oldCmpIds
          nonconsistentCmpIds <- c(nonconsistentCmpIds, nonconsistentCmpId)
          oldTstate <- updateClusterCnt(tconf, oldTstate, i)
          if (length(invisObsCmpIds) == 0)
          {
            break
          }
        }
      }
      #oldTstate$pts[oldTstate$pts$id %in% invisObsCmpIds,] <- newCmpObs[newCmpObs$id %in% invisObsCmpIds,]
      oldTstate <- updateClusterCnt(tconf, oldTstate, i)
      isNeedUpdateCnt <- FALSE
    }
    oldTstate$nonconsistentCmpAllIds <- c(oldTstate$nonconsistentCmpAllIds, nonconsistentCmpIds)
    return(oldTstate)
  }
  oldTstate$nonconsistentCmpAllIds <- integer() 
  if (length(oldTstate$cmps) > 0)
  {
    for (i in 1:length(oldTstate$cmps))
    {
      oldTstate <- updateExistingCluster(oldTstate, i)
    }
  }
  return(oldTstate)
}