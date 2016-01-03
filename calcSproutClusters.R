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
  maxPasses <- n*n*n
  for (passInd in 1:maxPasses) # Global attempts to process seed clusters.
  {
    if (passInd == maxPasses) # Maximal count of global iterations is reached.
    { #  Do super-super-force clustering: We make all seed clusters as forcely detachable.
      isForceClustering <- FALSE
      for (i in 1:n)
      {
        if (seedClusters$objs$cRank[i] != Inf)
        {
          stopifnot(seedClusters$objs$cRank[i] == 0.0)
          seedClusters$inds[[i]] <- c(i)
        }
      }
    }
    else if (isForceClustering) # Force clustrting mode is fail. Do super-force clustering:
    { # We make one seed cluster as forcely detachable.
      isForceClustering <- FALSE
      seedClusters$objs$cRank[seedClusters$objs$cRank == 0.0] <-
        seedClusters$objs$cRankPrev[seedClusters$objs$cRank == 0.0] # Revert all cRanks.
      cRankOrder <- order(seedClusters$objs$cRank)
      k <- cRankOrder[1]
      stopifnot(seedClusters$objs$cRank[k] != Inf)
      seedClusters$objs$cRank[k] <- 0.0
      seedClusters$inds[[k]] <- c(k)
    }
    else if (!isUpdated) # Processing the seed clusters has no change anyting. We should enable force clustering mode.
    {
      stopifnot(all(seedClusters$objs$cRank == Inf | seedClusters$objs$cRank == 0.0))
      seedClusters$objs$cRank[seedClusters$objs$cRank == 0.0] <-
        seedClusters$objs$cRankPrev[seedClusters$objs$cRank == 0.0] # Revert all cRanks.
      isForceClustering <- TRUE # Enable force clustering mode.
    }
    isUpdated <- FALSE
    cRankOrder <- order(seedClusters$objs$cRank)
    for (i in 1:n)
    {
      k <- cRankOrder[i]
      if (seedClusters$objs$cRank[k] == Inf) # We process all uprocessed seed clusters.
      {
        if (i == 1) # All seed clusters are already processed.
        {
          return(clusters) 
        }
        else # We process all uprocessed seed clusters in this global iteration.
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
          seedClusters$objs[clusterRefInd, tconf$cenInds] <- newClusterCen

          seedClusters$objs$cRank[seedClusterInds] <- 0.0
          isForceClustering <- FALSE
          isUpdated <- TRUE
          break
        }
      }
      if (seedClusters$objs$cRank[k] == 0.0)
      {
        if (calcIsSeedClusterDetachable(seedClusters, seedClusterInds))
        {
          clusterCount <- clusterCount + 1
          clusters$inds[[clusterCount]] <- seedClusterInds
          seedClusters$objs$cRank[seedClusterInds] <- Inf
          
          clusterTs <- seeds$objs$t[seedClusterInds]
          clusterRefInd <- seedClusterInds[which(clusterTs == max(clusterTs))][1]
          clusters$id[clusterCount] <- clusterCount
          clusters$x[clusterCount] <- seedClusters$objs$x[clusterRefInd]
          clusters$y[clusterCount] <- seedClusters$objs$y[clusterRefInd]
          clusters$t[clusterCount] <- seeds$objs$t[clusterRefInd]
          clusters$r[clusterCount] <- seeds$objs$r[clusterRefInd] # !!!! Must be changed !!!!
          
          seeds$d <- detachPoint(seeds$d, seedClusterInds)
          for (ind in which(seedClusters$objs$cRank != Inf))
          {
            if (length(intersect(seedClusters$inds[[ind]], seedClusterInds)) != 0)
            {
              seedCluster <- calcSeedCluster(tconf, seeds, ind)
              seedClusters$inds[[ind]] <- seedCluster$inds
              seedClusters$allInds[[ind]] <- seedCluster$allInds
              seedClusters$objs[ind, tconf$cenInds] <- seedCluster$cen
              seedClusters$objs$cRank[ind] <- seedCluster$cRank
              seedClusters$objs$cRankPrev[ind] <- seedCluster$cRank
            }
            else if (length(intersect(seedClusters$allInds[[ind]], seedClusterInds)) != 0)
            {
              seedClusters$allInds[[ind]] <- setdiff(seedClusters$allInds[[ind]], seedClusterInds)
              seedClusters$objs$cRank[ind] <- calcConflictingRank(tconf, seeds, seedClusters$inds[[ind]],
                                                             seedClusters$allInds[[ind]],
                                                             seedClusters$objs[ind,tconf$cenInds])
              seedClusters$objs$cRankPrev[ind] <- seedClusters$objs$cRank[ind]
            }
            else if (seedClusters$objs$cRank[ind] == 0.0 && seedClusters$objs$cRankPrev[ind] != 0.0)
            {
              seedClusters$objs$cRank[ind] <- seedClusters$objs$cRankPrev[ind]
            }
          }
          isUpdated <- TRUE
          break
        }
      }
      else
      {
        seedClusters$objs$cRank[k] <- 0.0
        isUpdated <- TRUE
        break
      }
    }
  }
  return(clusters)
}