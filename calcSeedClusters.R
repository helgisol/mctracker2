calcSeedClusters <- function(
  tconf,
  obs, # Observation data frame.
  p, # Point coordinates from observation data frame.
  d, # Distance map.
  w) # Radius growth time factor for time difference correction.
{
  clusters <- list()
  n <- nrow(obs) # Observed point count.
  for(i in 1:n)
  {
    clusters <- calcSeedCluster(clusters, obs, p, d, w, i, tconf$tol, tconf$dRdT)
  }
  return(clusters)
}