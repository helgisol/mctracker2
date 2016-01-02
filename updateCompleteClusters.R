updateCompleteClusters <- function(tconf, tstate, updClusters)
{
  stopifnot(tconf$groupCount > 0)
  if (length(updClusters$cmpIds) > 0)
  {
    completeClusterInds <- which(sapply(updClusters$cmpIds, function(x) length(x) == tconf$groupCount))
    tstate$cmpIds <- updClusters$cmpIds[completeClusterInds]
    tstate$objs <- updClusters$objs[completeClusterInds,]
  }
  else
  {
    tstate$cmpIds <- list()
    tstate$objs <- data.frame(id=integer(),x=double(),y=double(),t=double(),r=double())
  }
  return(tstate)
}