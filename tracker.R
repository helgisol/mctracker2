#obs <- sampleObs()
#a <- tracker(obs)

source('sampleObs.R')
source('distMapCalc.R')
source('initSeedClustersCalc.R')
source('seedClusterCalc.R')
source('clusterCenCalc.R')
source('conflictingRankCalc.R')
source('isSeedClusterDetachedCalc.R')

tracker <- function(obs)
{
  d <- distMapCalc(obs) # Distance map for all points. d[i,i] is filled, but distance for same group's point is NA.
  n <- nrow(obs) # Number of points.
  xyInds <- which(colnames(obs) == c("x","y")) # Point coordinate indices in observation data frame.
  tol <- 1e-3 # Tolerance for mean shift process breaking.

  clusters <- clusterCalc(obs, d, n, xyInds, tol)
}

clusterCalc <- function(obs, d, n, xyInds, tol)
{
  p <- obs[,xyInds] # Point coordinates from observation data frame.
  seedClusters <- initSeedClustersCalc(obs, p, d, tol)
  clusters <- list()
  clusterCount <- 0
  for(j in 1:n)
  {
    cRankOrder <- order(seedClusters$cRank)
    for(i in 1:2000)
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
      if (seedClusters$cRank[k] == 0.0)
      {
        if (isSeedClusterDetachedCalc(seedClusters, seedClusterInds))
        {
          clusterCount <- clusterCount + 1
          clusters$inds[[clusterCount]] <- seedClusterInds
          seedClusters$detached[seedClusterInds] <- TRUE
          seedClusters$cRank[seedClusterInds] <- Inf
          seedClusters$inds[!seedClusters$detached] <- lapply(seedClusters$inds[!seedClusters$detached], setdiff, y=seedClusterInds)
          break
        }
      }
      else
      {
        seedClusters <- seedClusterCalc(seedClusters, obs, p, d, k, seedClusters$prevInds[[k]], TRUE, tol)
        break
      }
    }
  }
  return(clusters)
}

distCalc <- function(pi,pj)
{
  sqrt( (pj$x-pi$x)^2 + (pj$y-pi$y)^2 )
}