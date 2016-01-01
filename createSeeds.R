createSeeds <- function(tconf, tstate)
{
  incompleteClusterIds <- 
    if (length(tstate$cmdIdsUpd) > 0)
    {
      tstate$objsUpd$id[sapply(tstate$cmdIdsUpd, function(x) length(x) > 0 && length(x) < tconf$groupCount)]
    }
  else
  {
    integer()
  }
  incompleteClusterObjs <- tstate$objsUpd[tstate$objsUpd$id %in% incompleteClusterIds, tconf$ixytrInds]
  newObsIds <- setdiff(tstate$obs$id, tstate$obss[[1]]$id) # Vector of ID's for pure new (previously non-existed) points.
  nonconsistentCmpObs <- tstate$obs[!(tstate$obs$id %in% newObsIds) && is.na(tstate$obs$tc), tconf$ixytrInds]
  newObs <- tstate$obs[tstate$obs$id %in% newObsIds,]
  
  objs <- rbind(
    cbind(
      incompleteClusterObjs,
      type = rep(1, nrow(incompleteClusterObjs))),
    cbind(
      nonconsistentCmpObs,
      type = rep(2, nrow(nonconsistentCmpObs))),
    cbind(
      newObs[,tconf$ixytrInds],
      type = rep(3, nrow(newObs))))

  
  seeds <- list(
    g = c(
      lapply(tstate$cmdIdsUpd[tstate$objsUpd$id %in% incompleteClusterIds], function(x) oldTstate$pts$g[x]),
      as.list(nonconsistentCmpObs$g),
      as.list(newObs$g)),
    objs = objs)
  
  seeds$d <- calcDistMap(tconf, seeds) # Distance map for all seeds.
  seeds$w <- calcClusterCenWeightMap(tconf, seeds) # Calculate weight map for cluster center calculation.
  
  # Update history for changed cluster objects.
  if (length(incompleteClusterIds) > 0)
  {
      for (id in incompleteClusterIds)
      {
        newCmpIds <- tstate$cmdIdsUpd[tstate$objsUpd$id == id]
        oldCmpIds <- tstate$cmdIds[tstate$objsid == id]
        if (!identical(newCmpIds, oldCmpIds))
        {
          newCmpObs <- tstate$obs[tstate$obs$id %in% newCmpIds,]
          visNewCmpIds <- newCmpObs$id[!is.na(newCmpObs$x)]
          for (lelel in 1:length(tstate$objss))
          {
            updatedObj <- tstate$objss[[level]][tstate$objss[[level]]$id == id, tconf$ixytrInds]
            tstate$objss[[level]][tstate$objss[[level]]$id == id, tconf$ixytrInds] <-
              updateClusterObj(tconf, updatedObj, visNewCmpIds, tstate$obss[[level]])
          }
        }
      }
  }
  
  return(seeds)
}