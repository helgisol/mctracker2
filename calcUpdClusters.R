calcUpdClusters <- function(tconf, tstate)
{
  updClusters <- list(
    ncCmpIds = integer(),
    unobsObjIds = integer(),
    cmpIds = list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double()))
  if (length(tstate$cmpIds) > 0)
  {
    updClusterList <- list()
    for (i in 1:length(tstate$cmpIds))
    {
      updClusterList[[i]] <- calcUpdCluster(tconf, tstate, tstate$cmpIds[[i]], tstate$objs[i,])
      if (all(is.na(tstate$obs$x[tstate$obs$id %in% updClusterList[[i]]$cmpIds])))
      {
        updClusters$unobsObjIds <- c(updClusters$unobsObjIds, tstate$objs$id[i])
      }
    }
    updClusters$ncCmpIds <- sapply(updClusterList, function(x) {x$ncCmpIds})
    # Filter deleted clusters.
    updClusterList <- updClusterList[lapply(updClusterList, function(x) {length(x$cmpIds)}) > 0]
    updClusters$cmpIds <- lapply(updClusterList, function(x) {x$cmpIds})
    updClusters$objs <- do.call(rbind.data.frame, lapply(updClusterList, function(x) {x$obj}) )
  }
  return(updClusters)
}