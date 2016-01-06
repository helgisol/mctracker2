# Calculating a conflicting rank for a seed cluster.
calcConflictingRank <- function(
  tconf,
  seeds,
  seedInd,
  clusterInds, # Indices of the closest (for each group) points in the cluster.
  clusterAllInds, # Indices of all points in the cluster.
  clusterCen) # Coordinates of cluster center (calculated for only closest points in the cluster).
{
  if (identical(clusterInds, clusterAllInds))
  {
    return(0.0) # There are not conflicting points in the cluster.
  }
  cRank <- 0.0 # Current value of resulted cRank.
  clusterOutInds <- setdiff(clusterAllInds, clusterInds)
  confSingleGroups <- unique(unlist(seeds$g[clusterOutInds]))
  clusterCenSeed <- seeds$objs[seedInd,]
  clusterCenSeed[,tconf$xyInds] <- clusterCen
  clusterInSeeds <- seeds$objs[clusterInds,]
  clusterOutSeeds <- seeds$objs[clusterOutInds,]
  distsIn <- calcSeedDist(tconf, seeds$dw[seedInd,clusterInds], clusterCenSeed, clusterInSeeds)
  distsOut <- calcSeedDist(tconf, seeds$dw[seedInd,clusterOutInds], clusterCenSeed, clusterOutSeeds)
  minDist <- min(distsIn)
  distsIn <- distsIn - minDist + 1.0
  distsOut <- distsOut - minDist + 1.0
  minDistDiff <- Inf
  for (i in 1:length(clusterInds))
  {
    indIn <- clusterInds[i]
    gIn <- seeds$g[[indIn]]
    if (length(intersect(gIn, confSingleGroups)) > 0)
    {
      for (j in 1:length(clusterOutInds))
      {
        indOut <- clusterOutInds[j]
        gOut <- seeds$g[[indOut]]
        if (length(intersect(gOut, gIn)) > 0)
        {
          distDiff <- abs(distsOut[j] - distsIn[i])
          if (distDiff < minDistDiff)
          {
            minDistDiff <- distDiff
            cRank <- distsIn[i] / distsOut[j]
          }
        }
      }
    }
  }
  return(cRank)
}