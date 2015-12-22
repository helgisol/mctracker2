distMapCalc <- function(
  obs, # Data frame wirh observations.
  timeFactor = 0.01) # Time factor for time difference correction.
{
  dm <- as.matrix(dist(cbind(obs$x, obs$y)))
  n <- nrow(obs)
  for(i in 1:n)
  {
    gi = obs$g[i]
    ri = obs$r[i]
    for(j in 1:n)
    {
      if (i == j)
      {
        dm[i,j] = - 2.0 * ri
      }
      else if (gi == obs$g[j])
      {
        dm[i,j] = NA
      }
      else
      {
        dm[i,j] = dm[i,j] - ri - (obs$r[j] + timeFactor * abs(obs$t[j] - obs$t[i]))
      }
    }
  }
  return(dm)
}