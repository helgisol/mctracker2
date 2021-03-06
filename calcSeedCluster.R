calcSeedCluster <- function(
  tconf,
  seeds,
  i) # Index of a seed object.
{
#   if (i == 15)
#   {
#     a <- 1
#   }
  ri = seeds$objs$r[i]
  ti = seeds$objs$t[i]
  origPt <- seeds$objs[i,tconf$xyInds]
  origSeed <- seeds$objs[i,]
  origMaxDist <- 2 * ri
  clusterAllInds <- c(i)
  clusterInds <- c(i)
  clusterCen <- origPt
  clusterCenSeed <- origSeed
  repeat
  {
    origShift <- calcPointDist(origPt, clusterCen) # Distance between original point and cluster's center.
    posClusterInds <- which(seeds$d[i,] <= origShift) # Indices (in global point list) of potential cluster points.
    posClusterCmpSeeds <- seeds$objs[posClusterInds,] # Seeds, which are potential cluster components.
    # Distances to cluster's center from all potential cluster's points.
    dists <- calcSeedDist(tconf, seeds$dw[i,posClusterInds], clusterCenSeed, posClusterCmpSeeds)
    newClusterAllInds <- posClusterInds[which(dists <= 0.0)]  # Indices (in global point list) of all cluster points.
    clusterGroups <- unlist(seeds$g[newClusterAllInds])
    if (length(unique(clusterGroups)) == length(clusterGroups)) # If there are no conflicted points in cluster.
    {
      newClusterInds <- newClusterAllInds  # Indices (in global point list) of only closes from conflicting points.
    }
    else
    {
      dists <- dists[which(dists <= 0.0)] # Distances for only points in cluster.
      distOrder <- order(dists)
      newClusterInds <- integer()
      newClusterGroups <- integer()
      for (j in distOrder)
      {
        jInd <- newClusterAllInds[j]
        jGroup <- seeds$g[[jInd]]
        if (length(intersect(newClusterGroups, jGroup)) == 0)
        {
          newClusterInds <- c(newClusterInds, jInd)
          newClusterGroups <- c(newClusterGroups, jGroup)
        }
      }
      newClusterInds <- sort(newClusterInds)
    }
    if (identical(clusterInds, newClusterInds))
    {
      if (!identical(clusterAllInds, newClusterAllInds))
      {
        clusterAllInds <- newClusterAllInds
      }
      break
    }
    newClusterPts <- seeds$objs[newClusterInds,tconf$xyInds] # Coordinates of new cluster's points.
    newClusterWs <- seeds$w[i,newClusterInds]
    newClusterCen <- calcClusterCnt(newClusterPts, newClusterWs) # Coordinates of new cluster's center.
    if (calcPointDist(newClusterCen, origPt) > origMaxDist)
    {
      break
    }
    clusterShift <- calcPointDist(newClusterCen, clusterCen)
    clusterAllInds <- newClusterAllInds
    clusterInds <- newClusterInds
    clusterCen <- newClusterCen
    clusterCenSeed[,tconf$xyInds] <- newClusterCen
    if (clusterShift < tconf$tol)
    {
      break
    }
  }
  cluster = list(
    inds = as.vector(clusterInds),
    allInds = as.vector(clusterAllInds),
    cen = clusterCen,
    cRank = calcConflictingRank(tconf, seeds, i, clusterInds, clusterAllInds, clusterCen))
  return(cluster)
}