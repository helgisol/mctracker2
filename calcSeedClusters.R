calcSeedClusters <- function(
  tconf,
  seeds)
{
  clusters <- list()
  for(i in 1:nrow(seeds$objs))
  {
    cluster <- calcSeedCluster(tconf, seeds, i)
    clusters$detached[i] = FALSE
    clusters$inds[[i]] <- cluster$inds
    clusters$allInds[[i]] <- cluster$allInds
    clusters$cen[[i]] <- cluster$cen
    clusters$cRank[i] <- cluster$cRank
    clusters$cRankPrev[i] <- cluster$cRank
  }
  return(clusters)
}