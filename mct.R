# Nonstandard conditions:
# 1. In obs there are all points for all groups (may be with the previous values).
# 2. In obs all points have globally unique IDs.
mct <- function(tconf, tstate, obs)
{
  tstate <- updateObss(tconf, tstate, obs)
  updClusters <- calcUpdClusters(tconf, tstate)
  tstate <- updateObjss(tconf, tstate, updClusters)
  seeds <- createSeeds(tconf, tstate, updClusters)
  sproutClusters <- calcSproutClusters(tconf, seeds)
  coordClusters <- coordinateClusters(tconf, tstate, seeds, sproutClusters)
  tstate <- updateCompleteClusters(tconf, tstate, updClusters)
  tstate <- updateCoordClusters(tconf, tstate, coordClusters)
  return(tstate)
}