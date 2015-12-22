initSeedClustersCalc <- function(
  obs, # Observation data frame.
  p, # Point coordinates from observation data frame.
  d, # Distance map.
  w, # Weight map for cluster center calculation.
  tol) # Tolerance for mean shift process breaking.
{
  clusters <- list()
  n <- nrow(obs) # Observed point count.
  for(i in 1:n)
  {
    clusters <- seedClusterCalc(clusters, obs, p, d, w, i, tol)
  }
  return(clusters)
}