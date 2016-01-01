calcClusterTcWeights <- function(tconf, tcs, minTc)
{
  tcDiffs <- abs(tcs - minTc)
  tcDiffsMax <- max(tcDiffs)
  if (tcDiffsMax < tconf$tolClusterTcDiffsMax)
  {
    newClusterTcW <- rep(tconf$clusterRFactorFirst, 1:length(tcs))
  }
  else
  {
    stopifnot(tconf$clusterRFactorFirst >= 1.0)
    newClusterTcW <- 1.0 + (tconf$clusterRFactorFirst - 1.0) * (1.0 - tcDiffs / tcDiffsMax)
  }
  return(newClusterTcW)
}