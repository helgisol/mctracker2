clusterCenCalc <- function(
  pts, # Points.
  ws) # Weights.
{
  lapply(pts, weighted.mean, ws)
}