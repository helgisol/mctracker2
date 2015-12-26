createSeeds <- function(tconf, oldTstate, newTstate)
{
  nonconsistentCmpIds <- updateExistingClusters(tconf, oldTstate, newTstate$pts)
  incompleteClusterIds <- 
    if (length(oldTstate$cmps) > 0)
    {
      oldTstate$objs$id[sapply(oldTstate$cmps, function(x) length(x) > 0 && length(x) < tconf$groupCount)]
    }
  else
  {
    integer()
  }
  newObsIds <- setdiff(newTstate$pts$id, oldTstate$pts$id) # Vector of ID's for pure new (previously non-existed) points.
  
  seeds <- list(
    g = c(
      lapply(oldTstate$cmps[incompleteClusterIds], function(x) oldTstate$pts$g[x]),
      as.list(oldTstate$pts$g[oldTstate$pts$id %in% nonconsistentCmpIds]),
      as.list(newTstate$pts$g[newTstate$pts$id %in% newObsIds])),
    objs = rbind(
      cbind(
        oldTstate$objs[newTstate$pts$id %in% incompleteClusterIds, tconf$ixytrInds],
        type = rep(1, length(incompleteClusterIds))),
      cbind(
        newTstate$pts[newTstate$pts$id %in% nonconsistentCmpIds, tconf$ixytrInds],
        type = rep(2, length(nonconsistentCmpIds))),
      cbind(
        newTstate$pts[newTstate$pts$id %in% newObsIds, tconf$ixytrInds],
        type = rep(3, length(newObsIds)))))
  return(seeds)
}