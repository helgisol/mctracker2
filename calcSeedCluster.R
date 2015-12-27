calcSeedCluster <- function(
  tconf,
  seeds,
  clusters, # List for all seed clusters.
  i) # Index of a seed object.
{
  if (i == 15)
  {
    a <- 1
  }
  p <- seeds$objs[,tconf$xyInds] # Point coordinates from observation data frame.
  ri = seeds$objs$r[i]
  ti = seeds$objs$t[i]
  origPt <- p[i,]
  origMaxDist <- 2 * ri
  clusters$detached[i] = FALSE
  clusterAllInds <- c(i)
  clusterInds <- c(i)
  clusterPts <- p[clusterInds,]
  clusterCen <- origPt
  repeat
  {
    origShift <- calcPointDist(origPt, clusterCen) # Distance between original point and cluster's center.
    posClusterInds <- which(seeds$d[i,] <= origShift) # Indices (in global point list) of potential cluster points.
    posClusterPts <- p[posClusterInds,] # Coordinates of potential cluster points.
    # Distances to cluster's center from all potential cluster's points.
    dists <- calcPointDist(clusterCen, posClusterPts)
    # Distances, corrected.
    dists <- dists - ri - (seeds$objs$r[posClusterInds] + tconf$dRdT * abs(seeds$objs$t[posClusterInds] - ti))
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
    newClusterPts <- p[newClusterInds,] # Coordinates of new cluster's points.
    newClusterWs <- seeds$w[i,newClusterInds]
    newClusterCen <- calcClusterCnt(newClusterPts, newClusterWs) # Coordinates of new cluster's center.
    if (calcPointDist(newClusterCen, origPt) > origMaxDist)
    {
      break
    }
    clusterShift <- calcPointDist(newClusterCen, clusterCen)
    clusterAllInds <- newClusterAllInds
    clusterInds <- newClusterInds
    clusterPts <- newClusterPts
    clusterCen <- newClusterCen
    if (clusterShift < tconf$tol)
    {
      break
    }
  }
  clusters$inds[[i]] <- as.vector(clusterInds)
  clusters$allInds[[i]] <- as.vector(clusterAllInds)
  clusters$cen[[i]] <- clusterCen
  clusters$cRank[i] <- calcConflictingRank(tconf, seeds, clusterInds, clusterAllInds, clusterCen)
  clusters$cRankPrev[i] <- clusters$cRank[i]
  return(clusters)
}