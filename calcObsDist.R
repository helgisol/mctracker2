# Calculate distance between two observations.
calcObsDist <- function(tconf, obsi, obsj)
{
  stopifnot(nrow(obsi) == 1)
  #ptDist <- calcPointDist(obsi[,tconf$xyInds], obsj[,tconf$xyInds])
  ptDist <- sqrt( (obsj$x-obsi$x)^2 + (obsj$y-obsi$y)^2 )
  ptDist - obsi$r - obsj$r - tconf$dRdT * abs(obsj$t - obsi$t)
}