updateExistingClusters <- function(tdata, obs)
{
  updateExistingCluster <- function(i)
  {
    oldCmpIds <- tdata$cmps[[i]] # Old component IDs.
    newCmpIds <- obs$id[obs$id %in% oldCmpIds] # New component IDs.
    
    oldCmpObs <- tdata$pts[tdata$pts$id %in% oldCmpIds,] # Old component observations.
    newCmpObs <- obs[obs$id %in% oldCmpIds,] # New component observations.

    visCmpIds <- oldCmpObs$id[!is.na(oldCmpObs$x)] # Visible component IDs (old non-hidden components).
    obsCmpIds <- newCmpObs$id[!is.na(newCmpObs$x)] # Observed component IDs (new non-hidden components).
    unvisCmpIds <- setdiff(oldCmpIds, visCmpIds) # Unvisible component IDs (old hidden components).
    unobsCmpIds <- setdiff(newCmpObs, obsCmpIds) # Unobserved component IDs (new hidden components).
    
    # Process deleted components.
    isNeedUpdateCnt <- FALSE
    deletedCmpIds <- setdiff(oldCmpIds, newCmpIds)
    if (length(deletedCmpIds) > 0)
    {
      tdata$cmps[[i]] <- newCmpIds
      isNeedUpdateCnt <- TRUE
    }
    
    # Process visible but unobserved component IDs.
    visUnobsCmpIds <- setdiff(visCmpIds, unobsCmpIds) # Visible but unobserved component IDs.
    if (length(visUnobsCmpIds) > 0)
    {
      tdata$pts$x[tdata$pts$id %in% visUnobsCmpIds,] <- NA
      isNeedUpdateCnt <- TRUE
    }
    
    # Process visible and observed component IDs.
    if (isNeedUpdateCnt)
    {
      updateClusterCnt(tdata, i)
    }
    
    
    
  }
  if (length(tdata$cmps) > 0)
  {
    for (i in 1:length(tdata$cmps))
    {
      updateExistingCluster(i)
    }
  }
}