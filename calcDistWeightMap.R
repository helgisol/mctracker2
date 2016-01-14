calcDistWeightMap <- function(
  tconf,
  seeds,
  histObj)
{
  n <- nrow(seeds$objs)
  dw <- matrix(NA,n,n)
  if (n > 0)
  {
    for(i in 1:n)
    {
      gi <- seeds$g[[i]]
      obji <- seeds$obj[i,]
      obj2i <- histObj[i,]
      for(j in 1:n)
      {
        if (i == j)
        {
          dw[i,j] <- 1.0
        }
        else if (length(intersect(gi,seeds$g[[j]])) > 0)
        {
          dw[i,j] <- NA
        }
        else
        {
          objj <- seeds$obj[j,]
          obj2j <- histObj[j,]
          if (is.na(obj2i$x) || is.na(obj2j$x))
          {
            dw[i,j] <- 1.0
          }
          else
          {
            dist <- calcObsDist(tconf, obji, objj)
            dist2 <- calcObsDist(tconf, obj2i, obj2j)
            if (dist2 <= dist)
            {
              dw[i,j] <- 1.0
            }
            else
            {
              if (dist <= 1e-5)
              {
                shift <- max(seeds$obj$r[i], seeds$obj$r[j])
                dist <- dist + shift
                dist2 <- dist2 + shift
              }
              dw[i,j] <- 1.0 + (dist2-dist)/dist * tconf$distWeightScaleFactor
            }
          }
        }
      }
    }    
  }
  return(dw)
}