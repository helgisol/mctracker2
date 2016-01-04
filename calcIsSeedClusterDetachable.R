calcIsSeedClusterDetachable <- function(tconf, seedClusters, inds)
{
  (length(inds) == 1) ||
    (all(seedClusters$objs$status[inds] %in% tconf$scStatus$freeOrQuazi) &&
       all(sapply(seedClusters$inds[inds], identical, inds)))
}