#obs <- sampleObs()
#a <- tracker(obs)

source('sampleObs.R')
source('distMapCalc.R')
source('clusterCenWeightMapCalc.R')
source('distCalc.R')
source('initSeedClustersCalc.R')
source('seedClusterCalc.R')
source('clusterCenCalc.R')
source('conflictingRankCalc.R')
source('isSeedClusterDetachableCalc.R')
source('detachPoint.R')

tracker <- function(obs)
{
  d <- distMapCalc(obs) # Distance map for all points. d[i,i] is filled, but distance for same group's point is NA.
  n <- nrow(obs) # Number of points.
  xyInds <- which(colnames(obs) == c("x","y")) # Point coordinate indices in observation data frame.
  tol <- 1e-3 # Tolerance for mean shift process breaking.
  w <- clusterCenWeightMapCalc(obs, d)

  clusters <- clusterCalc(obs, d, w, n, xyInds, tol)
}

clusterCalc <- function(obs, d, w, n, xyInds, tol)
{
  p <- obs[,xyInds] # Point coordinates from observation data frame.
  seedClusters <- initSeedClustersCalc(obs, p, d, w, tol)
  clusters <- list()
  clusterCount <- 0
  isForceClustering <- FALSE
  isUpdated <- TRUE
  for(j in 1:2000)
  {
    if (j == 1000)
    {
      a1 <- 1
    }
    if (isForceClustering)
    {
      isForceClustering <- FALSE
      for(i in 1:n)
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
    for(i in 1:n)
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
          seedClusters$cRank[seedClusterInds] <- 0.0
          isForceClustering <- FALSE
          isUpdated <- TRUE
          break
        }
      }
      if (seedClusters$cRank[k] == 0.0)
      {
        if (isSeedClusterDetachableCalc(seedClusters, seedClusterInds))
        {
          clusterCount <- clusterCount + 1
          clusters$inds[[clusterCount]] <- seedClusterInds
          seedClusters$detached[seedClusterInds] <- TRUE
          seedClusters$cRank[seedClusterInds] <- Inf
          d <- detachPoint(d, seedClusterInds)
          for (ind in which(!seedClusters$detached))
          {
            if (length(intersect(seedClusters$inds[[ind]], seedClusterInds)) != 0)
            {
              seedClusters <- seedClusterCalc(seedClusters, obs, p, d, w, ind, tol)
            }
            else if (length(intersect(seedClusters$allInds[[ind]], seedClusterInds)) != 0)
            {
              seedClusters$allInds[[ind]] <- setdiff(seedClusters$allInds[[ind]], seedClusterInds)
              seedClusters$cRank[ind] <- conflictingRankCalc(obs$g, p, seedClusters$inds[[ind]], seedClusters$allInds[[ind]], seedClusters$cen[[ind]])
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
