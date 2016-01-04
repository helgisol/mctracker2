updateCoordClusters <- function(tconf, tstate, coordClusters)
{
  tstate$lastId <- coordClusters$lastId
  tstate$cmpIds <- c(tstate$cmpIds, coordClusters$cmpIds)
  tstate$objs <- rbind(tstate$objs, coordClusters$objs)
  if (nrow(tstate$objs) > 0)
  {
    row.names(tstate$objs) <- 1:nrow(tstate$objs)
  }
  tstate$obs$tc[is.na(tstate$obs$tc)] <- tstate$obs$t[is.na(tstate$obs$tc)]
  return(tstate)
}