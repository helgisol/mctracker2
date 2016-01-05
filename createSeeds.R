createSeeds <- function(tconf, tstate, updClusters)
{
  incompleteClusterIds <- 
    if (length(updClusters$cmpIds) > 0)
    {
      setdiff(
        updClusters$objs$id[sapply(updClusters$cmpIds, function(x) length(x) > 0 && length(x) < tconf$groupCount)],
        updClusters$unobsObjIds)
    }
  else
  {
    integer()
  }
  incompleteClusterObjs <- updClusters$objs[updClusters$objs$id %in% incompleteClusterIds, tconf$ixytrInds]
  newObsIds <- setdiff(tstate$obs$id, tstate$obss[[1]]$id) # Vector of ID's for pure new (previously non-existed) points.
  nonconsistentCmpObs <- tstate$obs[tstate$obs$id %in% updClusters$ncCmpIds, tconf$ixytrInds]
  newObs <- tstate$obs[tstate$obs$id %in% newObsIds,]
  
  objs <- rbind(
    cbind(
      incompleteClusterObjs,
      s = rep(tconf$sSort$incompleteCluster, nrow(incompleteClusterObjs))),
    cbind(
      nonconsistentCmpObs,
      s = rep(tconf$sSort$nonconsistentCmp, nrow(nonconsistentCmpObs))),
    cbind(
      newObs[,tconf$ixytrInds],
      s = rep(tconf$sSort$newObs, nrow(newObs))))

  seeds <- list(
    g = c(
      lapply(updClusters$cmpIds[updClusters$objs$id %in% incompleteClusterIds], function(x) tstate$obs$g[x]),
      as.list(nonconsistentCmpObs$g),
      as.list(newObs$g)),
    objs = objs)
  
  seeds$d <- calcDistMap(tconf, seeds) # Distance map for all seeds.
  seeds$w <- calcClusterCenWeightMap(tconf, seeds) # Calculate weight map for cluster center calculation.
  
  return(seeds)
}