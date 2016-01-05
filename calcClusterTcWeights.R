calcClusterTcWeights <- function(tconf, tcs, minTc)
{
  tcDiffs <- abs(tcs - minTc)
  tcDiffsMax <- max(tcDiffs)
  if (tcDiffsMax < tconf$tolClusterTcDiffsMax)
  {
    newClusterTcW <- rep(1.0, length(tcs))
  }
  else
  {
    stopifnot(tconf$clusterRFactorTcMax >= 1.0)
    newClusterTcW <- 1.0 + (tconf$clusterRFactorTcMax - 1.0) * (1.0 - tcDiffs / tcDiffsMax)
  }
  return(newClusterTcW)
}