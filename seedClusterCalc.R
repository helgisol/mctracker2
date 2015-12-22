seedClusterCalc <- function(
  clusters, # List for all seed clusters.
  obs, # Observation data frame.
  p, # Point coordinates from observation data frame.
  d, # Distance map.
  i, # Index of a seed point.
  initClusterInds = NA, # Inintial set of clusterindices.
  doConflictEliminaton, # Flag for conflict eliminating (if TRUE, conflicted points are ignored, otherwise clastering breaks).
  tol) # Tolerance for mean shift process breaking.
{
  origPt <- p[i,]
  origMaxDist <- 2 * obs$r[i]
  clusters$detached[i] = FALSE
  clusters$cRank[i] = 0.0
  clusters$prevInds[[i]] <- c(i)
  if (is.na(initClusterInds))
  {
    clusterInds <- c(i)
    clusterPts <- p[clusterInds,]
    clusterCen <- origPt
  }
  else
  {
    stopifnot(doConflictEliminaton)
    stopifnot(length(obs$g[initClusterInds]) == length(initClusterInds))
    clusterInds <- initClusterInds
    clusterPts <- p[clusterInds,]
    clusterCen <- clusterCenCalc(clusterPts)
  }
  repeat
  {
    origShift <- distCalc(origPt, clusterCen) # Distance between original point and cluster's center.
    posClusterInds <- which(d[i,] <= origShift) # Indices (in global point list) of potential cluster points.
    posClusterPts <- p[posClusterInds,] # Coordinates of potential cluster points.
    dists <- distCalc(clusterCen, posClusterPts) # Distances to cluster's center from all potential cluster's points.
    dists <- (dists - obs$r[i]) - obs$r[posClusterInds] # Distances, corrected by point radii.
    newClusterInds <- posClusterInds[which(dists <= 0.0)]  # Indices (in global point list).
    if (doConflictEliminaton)
    {
      clusterGroups <- obs$g[newClusterInds]
      if (length(unique(clusterGroups)) != length(newClusterInds))
      {
        dists <- dists[which(dists <= 0.0)] # Distances for only points in cluster.
        distOrder <- order(dists)
        filterPosClusterLocInds <- distOrder[!duplicated(clusterGroups[distOrder])]
        newClusterInds <- newClusterInds[filterPosClusterLocInds]
      }
    }
    newClusterPts <- p[newClusterInds,] # Coordinates of new cluster's points.
    newClusterCen <- clusterCenCalc(newClusterPts) # Coordinates of new cluster's center.
    if (distCalc(newClusterCen, origPt) > origMaxDist)
    {
      break
    }
    clusterShift <- distCalc(newClusterCen, clusterCen)
    prevClusterInds <- clusterInds
    clusterInds <- newClusterInds
    clusterPts <- newClusterPts
    clusterCen <- newClusterCen
    clusterUniGroups <- unique(obs$g[newClusterInds])
    if (!doConflictEliminaton && length(clusterUniGroups) < length(newClusterInds))
    {
      clusters$cRank[i] <- conflictingRankCalc(obs$g, clusterInds, clusterPts, clusterCen, clusterUniGroups)
      clusters$prevInds[[i]] <- prevClusterInds
      break
    }
    if (clusterShift < tol)
    {
      break
    }
  }
  clusters$inds[[i]] <- clusterInds
  return(clusters)
}