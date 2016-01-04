updateSeedCluster <- function(tconf, seeds, seedClusters, ind)
{
  seedCluster <- calcSeedCluster(tconf, seeds, ind)
  seedClusters$inds[[ind]] <- seedCluster$inds
  seedClusters$allInds[[ind]] <- seedCluster$allInds
  seedClusters$objs[ind, tconf$cenInds] <- seedCluster$cen
  seedClusters$objs$cRank[ind] <- seedCluster$cRank
  seedClusters$objs$status[ind] <- ifelse(seedCluster$cRank == 0.0, tconf$scStatus$free, tconf$scStatus$cond)
  return(seedClusters) 
}