initSeedClustersCalc <- function(
  obs, # Observation data frame.
  p, # Point coordinates from observation data frame.
  d, # Distance map.
  tol) # Tolerance for mean shift process breaking.
{
  clusters <- list()
  n <- nrow(obs) # Observed point count.
  for(i in 1:n)
  {
    clusters <- seedClusterCalc(clusters, obs, p, d, i, tol)
  }
  return(clusters)
}