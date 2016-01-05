# Calculate distance between cluster's center (obj) and components' observations (obs).
calcClusterCmpDist <- function(tconf, obj, obs)
{
  stopifnot(nrow(obj) == 1)
  minTc <- min(obs$tc)
  obsTcW <- calcClusterTcWeights(tconf, obs$tc, minTc)
  ptDist <- sqrt( (obs$x-obj$x)^2 + (obs$y-obj$y)^2 )
  rFs <- tconf$clusterRFactorLost * obsTcW # radius factors.
  ptDist - obj$r - rFs * obs$r - tconf$dRdT * abs(obs$t - obj$t)
}