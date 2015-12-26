updateTstateForCompleteClusters <- function(tconf, oldTstate, newTstate)
{
  stopifnot(tconf$groupCount > 0)
  if (length(oldTstate$cmps) > 0)
  {
    clusterInds <- which(sapply(oldTstate$cmps, function(x) length(x) == tconf$groupCount))
    if (length(clusterInds) > 0)
    {
      newTstate$cmps <- c(newTstate$cmps, oldTstate$cmps[clusterInds])
      newTstate$objs <- rbind(newTstate$objs, oldTstate$objs[clusterInds,])
    }
  }
  return(newTstate)
}