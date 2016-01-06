# Calculate distance between two seeds.
calcSeedDist <- function(tconf, dw, seedi, seedj)
{
  stopifnot(nrow(seedi) == 1)
  #ptDist <- calcPointDist(seedi[,tconf$xyInds], seedj[,tconf$xyInds])
  ptDist <- sqrt( (seedj$x-seedi$x)^2 + (seedj$y-seedi$y)^2 )
  rFs <- tconf$sSortDistFactor[seedi$s,seedj$s] # radius factors.
  dw * ptDist - seedi$r - rFs * seedj$r - tconf$dRdT * abs(seedj$t - seedi$t)
}