# Nonstandard conditions:
# 1. In obs there are all points for all groups (may be with the previous values).
# 2. In obs all points have globally unique IDs.
mct <- function(tconf, tstate, obs)
{
  tstate <- updateObss(tconf, tstate, obs)
  tstate <- updateExistingClusters(tconf, tstate)
  seeds <- createSeeds(tconf, tstate)
  sproutClusters <- calcSproutClusters(tconf, seeds)
  coordClusters <- coordinateClusters(tconf, tstate, seeds, sproutClusters, tstate$lastId)
  
  tstate <- updateCompleteClusters(tconf, tstate)
  
  tstate$lastId <- max(coordClusters$objs$id)
  tstate$cmpIds <- c(tstate$cmpIds, coordClusters$cmpIds)
  tstate$objs <- rbind(tstate$objs, coordClusters$objs)
  if (nrow(tstate$objs) > 0)
  {
    row.names(tstate$objs) <- 1:nrow(tstate$objs)
  }

  return(tstate)
}