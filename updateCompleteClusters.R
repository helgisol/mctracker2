updateCompleteClusters <- function(tconf, tstate)
{
  stopifnot(tconf$groupCount > 0)
  if (length(tstate$cmdIdsUpd) > 0)
  {
    completeClusterInds <- which(sapply(tstate$cmdIdsUpd, function(x) length(x) == tconf$groupCount))
    tstate$cmpIds <- tstate$cmdIdsUpd[completeClusterInds]
    tstate$objs <- tstate$objsUpd[completeClusterInds,]
  }
  else
  {
    tstate$cmpIds <- list()
    tstate$objs <- data.frame(id=integer(),x=double(),y=double(),t=double(),r=double())
  }
  return(tstate)
}