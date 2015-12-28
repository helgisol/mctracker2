createSeeds <- function(tconf, oldTstate, newTstate, nonconsistentCmpIds, prevTstate)
{
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
  
  objs <- rbind(
    cbind(
      oldTstate$objs[oldTstate$objs$id %in% incompleteClusterIds, tconf$ixytrInds],
      type = rep(1, length(incompleteClusterIds))),
    cbind(
      newTstate$pts[newTstate$pts$id %in% nonconsistentCmpIds, tconf$ixytrInds],
      type = rep(2, length(nonconsistentCmpIds))),
    cbind(
      newTstate$pts[newTstate$pts$id %in% newObsIds, tconf$ixytrInds],
      type = rep(3, length(newObsIds))))
  
  objs1 <- rbind(
    cbind(
      prevTstate$objs[prevTstate$objs$id %in% incompleteClusterIds, tconf$ixytrInds],
      type = rep(1, length(incompleteClusterIds))),
    cbind(
      prevTstate$pts[prevTstate$pts$id %in% nonconsistentCmpIds, tconf$ixytrInds],
      type = rep(2, length(nonconsistentCmpIds))),
    data.frame(
      id=newObsIds,
      x=rep(NA,length(newObsIds)),
      y=rep(NA,length(newObsIds)),
      t=rep(NA,length(newObsIds)),
      r=rep(NA,length(newObsIds)),
      type=rep(3,length(newObsIds))))
  
  seeds <- list(
    g = c(
      lapply(oldTstate$cmps[oldTstate$objs$id %in% incompleteClusterIds], function(x) oldTstate$pts$g[x]),
      as.list(oldTstate$pts$g[oldTstate$pts$id %in% nonconsistentCmpIds]),
      as.list(newTstate$pts$g[newTstate$pts$id %in% newObsIds])),
    objs = objs,
    objs1 = objs1)
  return(seeds)
}