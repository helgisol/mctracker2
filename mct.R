# Nonstandard conditions:
# 1. In obs there are all points for all groups (may be with the previous values).
# 2. In obs all points have globally unique IDs.
mct <- function(tconf, tstate, obs)
{
  tstate <- updateObss(tconf, tstate, obs)
  tstate <- updateExistingClusters(tconf, tstate)
  seeds <- createSeeds(tconf, tstate)
  sproutClusters <- calcSproutClusters(tconf, seeds)
  coordClusters <- coordinateClusters(tconf, tstate, seeds, sproutClusters)
  tstate <- updateCompleteClusters(tconf, tstate)
  tstate <- updateCoordClusters(tconf, tstate, coordClusters)
  return(tstate)
}