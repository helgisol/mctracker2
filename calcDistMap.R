calcDistMap <- function( # Distance map for all seeds. d[i,i] is filled, but distance for same group's point is NA.
  tconf,
  seeds)
{
  dm <- as.matrix(dist(seeds$objs[,tconf$xyInds]))
  n <- nrow(seeds$objs)
  for(i in 1:n)
  {
    gi = seeds$g[[i]]
    ri = seeds$objs$r[i]
    ti = seeds$objs$t[i]
    for(j in 1:n)
    {
      if (i == j)
      {
        dm[i,j] = - 2.0 * ri
      }
      else if (length(intersect(gi,seeds$g[[j]])) > 0)
      {
        dm[i,j] = NA
      }
      else
      {
        rj = seeds$objs$r[j]
        tj = seeds$objs$t[j]
        dm[i,j] = dm[i,j] - ri - (rj + tconf$dRdT * abs(tj - ti))
      }
    }
  }
  return(dm)
}