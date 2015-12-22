seedClusterCalc <- function(
  clusters, # List for all seed clusters.
  obs, # Observation data frame.
  p, # Point coordinates from observation data frame.
  d, # Distance map.
  w, # Weight map for cluster center calculation.
  i, # Index of a seed point.
  tol) # Tolerance for mean shift process breaking.
{
#   if (i == 21)
#   {
#     a <- 1
#   }
  origPt <- p[i,]
  origMaxDist <- 2 * obs$r[i]
  clusters$detached[i] = FALSE
  clusterAllInds <- c(i)
  clusterInds <- c(i)
  clusterPts <- p[clusterInds,]
  clusterCen <- origPt
  repeat
  {
    origShift <- distCalc(origPt, clusterCen) # Distance between original point and cluster's center.
    posClusterInds <- which(d[i,] <= origShift) # Indices (in global point list) of potential cluster points.
    posClusterPts <- p[posClusterInds,] # Coordinates of potential cluster points.
    dists <- distCalc(clusterCen, posClusterPts) # Distances to cluster's center from all potential cluster's points.
    dists <- (dists - obs$r[i]) - obs$r[posClusterInds] # Distances, corrected by point radii.
    newClusterAllInds <- posClusterInds[which(dists <= 0.0)]  # Indices (in global point list) of all cluster points.
    clusterGroups <- obs$g[newClusterAllInds]
    if (length(unique(clusterGroups)) == length(newClusterAllInds)) # If there are no conflicted points in cluster.
    {
      newClusterInds <- newClusterAllInds  # Indices (in global point list) of only closes from conflicting points.
    }
    else
    {
      dists <- dists[which(dists <= 0.0)] # Distances for only points in cluster.
      distOrder <- order(dists)
      filterPosClusterLocInds <- distOrder[!duplicated(clusterGroups[distOrder])]
      newClusterInds <- sort(newClusterAllInds[filterPosClusterLocInds])
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
    newClusterWs <- d[i,newClusterInds]
    newClusterCen <- clusterCenCalc(newClusterPts, newClusterWs) # Coordinates of new cluster's center.
    if (distCalc(newClusterCen, origPt) > origMaxDist)
    {
      break
    }
    clusterShift <- distCalc(newClusterCen, clusterCen)
    clusterAllInds <- newClusterAllInds
    clusterInds <- newClusterInds
    clusterPts <- newClusterPts
    clusterCen <- newClusterCen
    if (clusterShift < tol)
    {
      break
    }
  }
  clusters$inds[[i]] <- as.vector(clusterInds)
  clusters$allInds[[i]] <- as.vector(clusterAllInds)
  clusters$cen[[i]] <- clusterCen
  clusters$cRank[i] <- conflictingRankCalc(obs$g, p, clusterInds, clusterAllInds, clusterCen)
  clusters$cRankPrev[i] <- clusters$cRank[i]
  return(clusters)
}