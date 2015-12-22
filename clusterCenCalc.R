clusterCenCalc <- function(
  pts, # Points.
  ws) # Weights.
{
  ws <- ws / norm(ws, type="2")
  
  as.data.frame(as.list(colMeans(pts)))
}