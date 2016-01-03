calcSeedClusters <- function(
  tconf,
  seeds)
{
  seedClusterList <- list()
  for(i in 1:nrow(seeds$objs))
  {
    seedCluster <- calcSeedCluster(tconf, seeds, i)
    seedClusterList[[i]] <- seedCluster
  }
  cens <- as.data.frame(do.call(rbind, lapply(seedClusterList, function(a) {a$cen})))
  cRank <- sapply(seedClusterList, function(a) {a$cRank})
  objs <- as.data.frame(cbind(x=unlist(cens$x), y=unlist(cens$y), cRank, cRankPrev=cRank))
  seedClusters <- list(
    inds = lapply(seedClusterList, function(a) {a$inds}),
    allInds = lapply(seedClusterList, function(a) {a$allInds}),
    objs = objs
  )
  return(seedClusters)
}