mct <- function(tconf, oldTstate, obs)
{
  #obsNewIds <- setdiff(obs$id, oldTstate$pts$id) # Vector of ID's for new points.
  #obsDelIds <- setdiff(oldTstate$pts$id, obs$id) # Vector of ID's for deleted points.
  #obsExiIds <- setdiff(obs$id, obsNewIds) # Vector of ID's for existed points.
  #obsHidIds <- obsExiIds[is.na(obsExiIds$x)] # Vector of ID's for hidden points.

  newTstate <- list(
    cmps = list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double()))
  newTstate$pts <- obs
  newTstate$n <- nrow(obs) # Number of points.
  
  newTstate <- updateTstateForCompleteClusters(tconf, oldTstate, newTstate)
  
  nonconsistentCmpIds <- updateExistingClusters(tconf, oldTstate, obs)
  incompleteClusterIds <- 
    if (length(oldTstate$cmps) > 0)
    {
      oldTstate$objs$id[sapply(oldTstate$cmps, function(x) length(x) > 0 && length(x) < tconf$groupCount)]
    }
    else
    {
      integer()
    }
  newObsIds <- setdiff(obs$id, oldTstate$pts$id) # Vector of ID's for pure new (previously non-existed) points.
  
  idMap <- cbind(
    type = c(rep(1, length(incompleteClusterIds)), rep(2, length(nonconsistentCmpIds)), rep(3, length(newObsIds))),
    id = c(incompleteClusterIds, nonconsistentCmpIds, newObsIds))
  idGroups <- c(
    lapply(oldTstate$cmps[incompleteClusterIds], function(x) oldTstate$pts$g[x]),
    as.list(oldTstate$pts$g[oldTstate$pts$id %in% nonconsistentCmpIds]),
    as.list(newTstate$pts$g[newTstate$pts$id %in% newObsIds]))

  newTstate$d <- distMapCalc(obs, idMap, tconf$dRdT) # Distance map for all points. d[i,i] is filled, but distance for same group's point is NA.
  newTstate$w <- clusterCenWeightMapCalc(obs, newTstate$d, tconf$dRdT) # Calculate weight map for cluster center calculation.

  clusters <- clusterCalc(tconf, obs, newTstate$d, newTstate$w, newTstate$n)
  components <- lapply(clusters$inds, function(x) obs$id[x])
  objects <- data.frame(
    id = clusters$id,
    x = clusters$x,
    y = clusters$y,
    t = clusters$t,
    r = clusters$r);

  newTstate$cmps <- components
  newTstate$objs <- objects
  
  return(newTstate)
}