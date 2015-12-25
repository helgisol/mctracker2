clusterCenWeightMapCalc <- function(
  obs, # Observation data frame.
  d, # Distance map.
  dRdT = 0.01) # Radius growth time factor for time difference correction.
{
  w <- d
  n <- nrow(d)
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      if (!is.na(d[i,j]))
      {
        w[i,j] <- obs$r[i] / (obs$r[j] + dRdT * abs(obs$t[j] - obs$t[i]))
      }
    }
  }
  return(w)
}