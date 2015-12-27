calcSeedClusters <- function(
  tconf,
  seeds)
{
  clusters <- list()
  for(i in 1:nrow(seeds$objs))
  {
    clusters <- calcSeedCluster(tconf, seeds, clusters, i)
  }
  return(clusters)
}