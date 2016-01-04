addSproutCluster <- function(seeds, seedClusters, sproutClusters, seedClusterInds, ind)
{
  ts <- seeds$objs$t[seedClusterInds]
  tMax <- max(ts)
  refPtInd <- seedClusterInds[which(ts == tMax)][1]
  ws <- seeds$w[refPtInd, seedClusterInds]
  sproutClusters$inds[[ind]] <- seedClusterInds
  sproutClusters$id[ind] <- ind
  sproutClusters$x[ind] <- seedClusters$objs$x[refPtInd]
  sproutClusters$y[ind] <- seedClusters$objs$y[refPtInd]
  sproutClusters$t[ind] <- seeds$objs$t[refPtInd]
  sproutClusters$r[ind] <- max(seeds$objs$r[refPtInd], weighted.mean(seeds$objs$r[seedClusterInds], ws))
  return(sproutClusters)
}