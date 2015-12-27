calcSproutClusters <- function(
  tconf,
  seeds)
{
  clusters <- list()
  seedClusters <- calcSeedClusters(tconf, seeds)
  clusterCount <- 0
  isForceClustering <- FALSE
  isUpdated <- TRUE
  n <- nrow(seeds$objs)
  for (j in 1:2000)
  {
    if (j == 1000)
    {
      a1 <- 1
    }
    if (isForceClustering)
    {
      isForceClustering <- FALSE
      for (i in 1:n)
      {
        if (seedClusters$cRank[i] != Inf)
        {
          seedClusters$cRank[i] <- 0.0
          seedClusters$inds[[i]] <- c(i)
          isUpdated <- TRUE
          break
        }
      }
    }
    else if (!isUpdated)
    {
      stopifnot(all(seedClusters$cRank == Inf | seedClusters$cRank == 0.0))
      seedClusters$cRank[seedClusters$cRank == 0.0] <- seedClusters$cRankPrev[seedClusters$cRank == 0.0]
      isForceClustering <- TRUE
    }
    isUpdated <- FALSE
    cRankOrder <- order(seedClusters$cRank)
    for (i in 1:n)
    {
      k <- cRankOrder[i]
      if (seedClusters$cRank[k] == Inf)
      {
        if (i == 1)
        {
          return(clusters)
        }
        else
        {
          break
        }
      }
      seedClusterInds <- seedClusters$inds[[k]]
      stopifnot(k %in% seedClusterInds)
      if (isForceClustering)
      {
        allInds <- seedClusters$allInds[seedClusterInds]
        stopifnot(length(allInds) > 1)
        isIncluded <- TRUE
        for(i0 in 1:length(allInds))
        {
          if (!identical(intersect(seedClusterInds, allInds[[i0]]), seedClusterInds))
          {
            isIncluded <- FALSE
            break
          }
        }
        if (isIncluded)
        {
          for(i0 in seedClusterInds)
          {
            seedClusters$inds[[i0]] <- seedClusterInds
          }
          
          clusterTs <- seeds$objs$t[seedClusterInds]
          clusterRefInd <- seedClusterInds[which(clusterTs == max(clusterTs))][1]
          newClusterPts <- seeds$objs[seedClusterInds, tconf$xyInds] # Coordinates of new cluster's points.
          newClusterWs <- seeds$w[clusterRefInd, seedClusterInds]
          newClusterCen <- calcClusterCnt(newClusterPts, newClusterWs) # Coordinates of new cluster's center.
          seedClusters$cen[[clusterRefInd]] <- newClusterCen

          seedClusters$cRank[seedClusterInds] <- 0.0
          isForceClustering <- FALSE
          isUpdated <- TRUE
          break
        }
      }
      if (seedClusters$cRank[k] == 0.0)
      {
        if (calcIsSeedClusterDetachable(seedClusters, seedClusterInds))
        {
          clusterCount <- clusterCount + 1
          clusters$inds[[clusterCount]] <- seedClusterInds
          seedClusters$detached[seedClusterInds] <- TRUE
          seedClusters$cRank[seedClusterInds] <- Inf
          
          clusterTs <- seeds$objs$t[seedClusterInds]
          clusterRefInd <- seedClusterInds[which(clusterTs == max(clusterTs))][1]
          clusters$id[clusterCount] <- clusterCount
          clusters$x[clusterCount] <- seedClusters$cen[[clusterRefInd]]$x
          clusters$y[clusterCount] <- seedClusters$cen[[clusterRefInd]]$y
          clusters$t[clusterCount] <- seeds$objs$t[clusterRefInd]
          clusters$r[clusterCount] <- seeds$objs$r[clusterRefInd] # !!!! Must be changed !!!!
          
          seeds$d <- detachPoint(seeds$d, seedClusterInds)
          for (ind in which(!seedClusters$detached))
          {
            if (length(intersect(seedClusters$inds[[ind]], seedClusterInds)) != 0)
            {
              seedClusters <- calcSeedCluster(tconf, seeds, seedClusters, ind)
            }
            else if (length(intersect(seedClusters$allInds[[ind]], seedClusterInds)) != 0)
            {
              seedClusters$allInds[[ind]] <- setdiff(seedClusters$allInds[[ind]], seedClusterInds)
              seedClusters$cRank[ind] <- calcConflictingRank(tconf, seeds, seedClusters$inds[[ind]], seedClusters$allInds[[ind]], seedClusters$cen[[ind]])
              seedClusters$cRankPrev[ind] <- seedClusters$cRank[ind]
            }
            else if (seedClusters$cRank[ind] == 0.0 && seedClusters$cRankPrev[ind] != 0.0)
            {
              seedClusters$cRank[ind] <- seedClusters$cRankPrev[ind]
            }
          }
          isUpdated <- TRUE
          break
        }
      }
      else
      {
        seedClusters$cRank[k] <- 0.0
        isUpdated <- TRUE
        break
      }
    }
  }
  return(clusters)
}