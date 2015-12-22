clusterCenWeightMapCalc <- function(
  obs,
  d, # Distance map.
  minTimeStep = 0.05) # Typical minimal time difference.
{
  timeFactor <- 1.0 / minTimeStep
  w <- d
  n <- nrow(d)
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      if (!is.na(d[i,j]))
      {
        w[i,j] = 1.0 / (1.0 + timeFactor * abs(obs$t[j] - obs$t[i]))
      }
    }
  }  
}