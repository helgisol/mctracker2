mct <- function(tconf, oldTstate, obs)
{
  newTstate <- list(
    cmps = list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double()))
  newTstate$pts <- obs
  newTstate$lastId <- oldTstate$lastId
  prevTstate <- oldTstate
  
  oldTstate <- updateExistingClusters(tconf, oldTstate, newTstate$pts)
  nonconsistentCmpIds <- oldTstate$nonconsistentCmpAllIds
  newTstate <- updateTstateForCompleteClusters(tconf, oldTstate, newTstate)

  seeds <- createSeeds(tconf, oldTstate, newTstate, nonconsistentCmpIds, prevTstate)
  seeds$d <- calcDistMap(tconf, seeds) # Distance map for all seeds.
  seeds$w <- calcClusterCenWeightMap(tconf, seeds) # Calculate weight map for cluster center calculation.

  sproutClusters <- calcSproutClusters(tconf, seeds)
  coordClusters <- coordinateClusters(tconf, oldTstate, seeds, sproutClusters, newTstate$lastId)

  newTstate$lastId <- max(coordClusters$objs$id)
  newTstate$cmps <- c(newTstate$cmps, coordClusters$cmps)
  newTstate$objs <- rbind(newTstate$objs, coordClusters$objs)
  if (nrow(newTstate$objs) > 0)
  {
    row.names(newTstate$objs) <- 1:nrow(newTstate$objs)
  }

  return(newTstate)
}