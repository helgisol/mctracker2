calcSproutClusters <- function(tconf, seeds)
{
  clusters <- list() # Resulted sprout clusters.
  n <- nrow(seeds$objs)
  if (n == 0)
  {
    return(clusters)
  }
  clusterCount <- 0 # Sprout cluster's counter.
  seedClusters <- calcSeedClusters(tconf, seeds) # Seed clusters.
  isUpdated <- FALSE # Flag for significant changes in seed cluster's structure during global iteration.
  isDetached <- FALSE # Flag for minor changes in seed cluster's structure during global iteration.
  
  datachSeedClusters <- function(seedClusterInds) # Subfunction for detaching group of seed clusters.
  {
    clusterCount <<- clusterCount + 1
    clusters <<- addSproutCluster(seeds, seedClusters, clusters, seedClusterInds, clusterCount)
    seedClusters$objs$status[seedClusterInds] <<- tconf$scStatus$detached
    seedClusters$objs$cRank[seedClusterInds] <<- Inf
    seeds$d <<- detachPoint(seeds$d, seedClusterInds)
    isChanged <- FALSE
    for (ind in which(seedClusters$objs$status != tconf$scStatus$detached))
    {
      if (calcIsIntersected(seedClusters$inds[[ind]], seedClusterInds))
      {
        seedClusters <<- updateSeedCluster(tconf, seeds, seedClusters, ind)
        isChanged <- TRUE
      }
      else if (calcIsIntersected(seedClusters$allInds[[ind]], seedClusterInds))
      {
        oldCrank <- seedClusters$objs$cRank[ind]
        seedClusters$allInds[[ind]] <<- setdiff(seedClusters$allInds[[ind]], seedClusterInds)
        seedClusters$objs$cRank[ind] <<- calcConflictingRank(tconf, seeds, ind, seedClusters$inds[[ind]],
                                                            seedClusters$allInds[[ind]],
                                                            seedClusters$objs[ind,tconf$cenInds])
        if (oldCrank != seedClusters$objs$cRank[ind])
        {
          isChanged <- TRUE
        }
      }
    }
    if (isChanged)
    {
      seedClusters$objs$status[seedClusters$objs$status == tconf$scStatus$quaziFree] <<- tconf$scStatus$cond
      isUpdated <<- TRUE
      return(TRUE)
    }
    else
    {
      isDetached <<- TRUE
    }
    return(FALSE)
  }
  
  maxPasses <- n^3
  for (passInd in 1:maxPasses) # Global attempts to process seed clusters.
  {
    if (passInd == maxPasses) # Maximal count of global iterations is reached.
    { #  Do super-super-force clustering: We make all seed clusters as forcely detachable.
      for (i in 1:n)
      {
        if (seedClusters$objs$status[i] != tconf$scStatus$detached)
        {
          seedClusterInds <- c(i)
          clusterCount <- clusterCount + 1
          clusters <- addSproutCluster(seeds, seedClusters, clusters, seedClusterInds, clusterCount)
        }
      }
      return(clusters)
    }
    isUpdated <- FALSE
    isDetached <- FALSE
    cRankOrder <- order(seedClusters$objs$cRank)
    for (i in 1:n)
    {
      k <- cRankOrder[i]
      if (seedClusters$objs$status[k] == tconf$scStatus$detached) # We process all uprocessed seed clusters.
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
      if (seedClusters$objs$status[k] == tconf$scStatus$cond)
      {
        seedClusters$objs$status[k] <- tconf$scStatus$quaziFree
        if (length(seedClusterInds) > 1)
        {
          seedClusterExeptSelfInds <- setdiff(seedClusterInds, k)
          if (any(seedClusters$objs$status[seedClusterExeptSelfInds] %in% tconf$scStatus$freeOrQuazi))
          {
            isUpdated <- TRUE
            break
          }
          else
          {
            next
          }
        }
      }
      if (calcIsSeedClusterDetachable(tconf, seedClusters, seedClusterInds))
      {
        if (datachSeedClusters(seedClusterInds))
        {
          break
        }
      }
    }
    if (!isUpdated) # Now we detach seed clusters, if the first from them has inds which are subset of all allInds.
    {
      if (isDetached)
      {
        isUpdated <- TRUE
        next
      }
      stopifnot(all(seedClusters$objs$status != tconf$scStatus$cond))
      stopifnot(any(seedClusters$objs$status != tconf$scStatus$detached))
      for (i in 1:n)
      {
        k <- cRankOrder[i]
        if (seedClusters$objs$status[k] == tconf$scStatus$detached) # We process all uprocessed seed clusters.
        {
          break
        }
        seedClusterInds <- seedClusters$inds[[k]]
        if (all(sapply(seedClusters$allInds[seedClusterInds], calcIsSubsetOf, seedClusterInds)))
        {
          seedClusters <- updateSeedClustersCen(tconf, seeds, seedClusters, seedClusterInds)
          if (datachSeedClusters(seedClusterInds))
          {
            break
          }
        }
      }
    }
    if (!isUpdated) # Now we detach single seed clusters starting from maximal cRank.
    {
      if (isDetached)
      {
        isUpdated <- TRUE
        next
      }
      stopifnot(all(seedClusters$objs$status != tconf$scStatus$cond))
      stopifnot(any(seedClusters$objs$status != tconf$scStatus$detached))
      ndCount <- sum(seedClusters$objs$status != tconf$scStatus$detached)
      for (i in 1:ndCount)
      {
        notDetachedCranks <- seedClusters$objs$cRank[seedClusters$objs$status != tconf$scStatus$detached]
        stopifnot(length(notDetachedCranks) > 0)
        maxCrank <- max(notDetachedCranks)
        k <- which(seedClusters$objs$cRank == maxCrank)
        stopifnot(seedClusters$objs$status[k] != tconf$scStatus$detached)
        seedClusterInds <- c(k)
        if (datachSeedClusters(seedClusterInds))
        {
          break
        }
      }
    }
  }
  return(clusters)
}