updateSeedClustersCen <- function(tconf, seeds, seedClusters, seedClusterInds)
{
  ts <- seeds$objs$t[seedClusterInds]
  refPtInd <- seedClusterInds[which(ts == max(ts))][1]
  clusterPts <- seeds$objs[seedClusterInds, tconf$xyInds] # Coordinates of new cluster's points.
  clusterWs <- seeds$w[refPtInd, seedClusterInds]
  seedClusters$objs[refPtInd, tconf$cenInds] <- calcClusterCnt(clusterPts, clusterWs)
  return(seedClusters)
}