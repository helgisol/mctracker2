# Calculate euclidian distance between two 2D-points.
# Points `pi` and `pj` would be lists with fields `x` and `y`.
calcPointDist <- function(pi,pj)
{
  sqrt( (pj$x-pi$x)^2 + (pj$y-pi$y)^2 )
}