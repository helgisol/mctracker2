calcDistMap <- function( # Distance map for all seeds. d[i,i] is filled, but distance for same group's point is NA.
  tconf,
  seeds)
{
  dm <- as.matrix(dist(seeds$objs[,tconf$xyInds]))
  n <- nrow(seeds$objs)
  if (n > 0)
  {
    for(i in 1:n)
    {
      gi <- seeds$g[[i]]
      seedi <- seeds$objs[i,]
      for(j in 1:n)
      {
        if (i == j)
        {
          dm[i,j] <- - 2.0 * seedi$r
        }
        else if (length(intersect(gi,seeds$g[[j]])) > 0)
        {
          dm[i,j] <- NA
        }
        else
        {
          seedj <- seeds$objs[j,]
          dm[i,j] <- calcSeedDist(tconf, seeds$dw[i,j], seedi, seedj)
        }
      }
    }
  }
  return(dm)
}