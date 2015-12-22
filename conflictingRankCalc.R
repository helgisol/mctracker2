conflictingRankCalc <- function(g, clusterInds, clusterPts, clusterCen, clusterGroups)
{
  dists <- distCalc(clusterCen, clusterPts) # Distances between a cluster's center and all cluster's points.
  cRank <- 0.0 # Current value of resulted cRank.
  for (group in clusterGroups) # For each group of points in cluster.
  {
    distsGroupped <- dists[obs$g[clusterInds] == group] # Select distances for only points from current group.
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