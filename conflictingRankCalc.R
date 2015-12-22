conflictingRankCalc <- function(
  g,
  p,
  clusterInds,
  clusterAllInds,
  clusterCen) # Coordinates of cluster center (calculated for obly closest points in cluster)
{
  if (identical(clusterInds, clusterAllInds))
  {
    return(0.0)
  }
  cRank <- 0.0 # Current value of resulted cRank.
  clusterAllPts <- p[clusterAllInds,] # Coordinates for every cluster points (conflicted points too).
  dists <- distCalc(clusterCen, clusterAllPts) # Distances between a cluster's center and all cluster's points.
  clusterGroups <- g[clusterAllInds] # Group index for every cluster points (conflicted points too).
  clusterUniGroups <- unique(clusterGroups)
  for (group in clusterUniGroups) # For each group of points in cluster.
  {
    distsGroupped <- dists[clusterGroups == group] # Select distances for only points from current group.
    if (length(distsGroupped) > 1) # Process only conflictiong groups.
    {
      distsGrouppedSorted <- sort(distsGroupped)
      cRankGroupped <- distsGrouppedSorted[1] / distsGrouppedSorted[2]
      if (cRankGroupped > 1.0)
      {
        cRankGroupped <- 1.0
      }
      if (cRankGroupped > cRank)
      {
        cRank <- cRankGroupped
      }
    }
  }
  return(cRank)
}