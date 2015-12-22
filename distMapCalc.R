distMapCalc <- function(obs)
{
  dm <- as.matrix(dist(cbind(obs$x, obs$y)))
  n <- nrow(obs)
  #gMask <- outer(1:n, 1:n, function(i,j) obs$g[i] == obs$g[j]); dm[gMask] <- NA
  for(i in 1:n)
  {
    gi = obs$g[i]
    ri = obs$r[i]
    for(j in 1:n)
    {
      if (i == j)
      {
        dm[i,j] = - ri - obs$r[j]
      }
      else if (gi == obs$g[j])
      {
        dm[i,j] = NA
      }
      else
      {
        dm[i,j] = dm[i,j] - ri - obs$r[j]
      }
    }
  }
  return(dm)

  # Another variant of calculation.
  d <- matrix(NA, n, n)
  for(i in 1:(n-1))
  {
    gi = obs$g[i]
    for(j in (i+1):n)
    {
      if (gi != obs$g[j])
      {
        d[i,j] = dm[i,j]
      }
    }
  }
  for(i in 2:n)
  {
    for(j in 1:(i-1))
    {
      d[i,j] = d[j,i]
    }
  }
  return(d)
}