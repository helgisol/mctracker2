# Calculating a conflicting rank for a seed cluster.
calcConflictingRank <- function(
  tconf,
  seeds,
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
  clusterInPts <- seeds$objs[clusterInds, tconf$xyInds]
  clusterOutPts <- seeds$objs[clusterOutInds, tconf$xyInds]
  distsIn <- calcPointDist(clusterCen, clusterInPts)
  distsOut <- calcPointDist(clusterCen, clusterOutPts)
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
          if (distDiff < 1e-5 && abs(distsOut[i]) < 1e-5)
          {
            return(1.0)
          }
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