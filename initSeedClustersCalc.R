initSeedClustersCalc <- function(
  obs, # Observation data frame.
  p, # Point coordinates from observation data frame.
  d, # Distance map.
  tol) # Tolerance for mean shift process breaking.
{
  clusters <- list()
  initClusterInds <- NA
  doConflictEliminaton <- FALSE
  n <- nrow(obs) # Observed point count.
  for(i in 1:n)
  {
    clusters <- seedClusterCalc(clusters, obs, p, d, i, initClusterInds, doConflictEliminaton, tol)
  }
  return(clusters)
}