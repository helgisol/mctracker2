calcConflictingRank <- function(
  tconf,
  seeds,
  clusterInds,
  clusterAllInds,
  clusterCen) # Coordinates of cluster center (calculated for obly closest points in cluster)
{
  if (identical(clusterInds, clusterAllInds))
  {
    return(0.0)
  }
  cRank <- 0.0 # Current value of resulted cRank.
  confSingleGroups <- unique(unlist(seeds$g[clusterAllInds]))
  clusterOutInds <- setdiff(clusterAllInds, clusterInds)
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