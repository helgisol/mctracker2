updateExistingClusters <- function(tconf, tstate)
{
  updClusters <- list()
  if (length(tstate$cmps) > 0)
  {
    for (i in 1:length(tstate$cmps))
    {
      if (i == 2)
      {
        q1 <- 1
      }
      updClusters[[i]] <- calcUpdCluster(tconf, tstate, tstate$cmpIds[[i]], tstate$objs[i,])
      tstate$obs$tc[tstate$obs$id %in% updClusters[[i]]$ncCmpIds] <- NA
    }
    updClusters <- updClusters[lapply(updClusters, function(x) {length(x$cmpIds)}) > 0]
    tstate$cmpIdsUpd <- lapply(updClusters, function(x) {x$cmpIds})
    tstate$objsUpd <- do.call(rbind.data.frame, lapply(updClusters, function(x) {x$obj}) )
  }
  else
  {
    tstate$cmpIdsUpd <- tstate$cmpIds
    tstate$objsUpd <- tstate$objs
    
  }
  return(tstate)
}