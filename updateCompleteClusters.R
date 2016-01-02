updateCompleteClusters <- function(tconf, tstate)
{
  stopifnot(tconf$groupCount > 0)
  if (length(tstate$cmpIdsUpd) > 0)
  {
    completeClusterInds <- which(sapply(tstate$cmpIdsUpd, function(x) length(x) == tconf$groupCount))
    tstate$cmpIds <- tstate$cmpIdsUpd[completeClusterInds]
    tstate$objs <- tstate$objsUpd[completeClusterInds,]
  }
  else
  {
    tstate$cmpIds <- list()
    tstate$objs <- data.frame(id=integer(),x=double(),y=double(),t=double(),r=double())
  }
  return(tstate)
}