calcClusterCenWeightMap <- function(
  tconf,
  seeds,
  d) # Distance map.
{
  w <- d
  n <- nrow(d)
  for(i in 1:n)
  {
    ri = seeds$objs$r[i]
    ti = seeds$objs$t[i]
    for(j in 1:n)
    {
      if (!is.na(d[i,j]))
      {
        rj = seeds$objs$r[j]
        tj = seeds$objs$t[j]
        w[i,j] <- ri / (rj + tconf$dRdT * abs(tj - ti))
      }
    }
  }
  return(w)
}