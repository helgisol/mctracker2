# Update history of cluster objects (objss).
updateObjss <- function(tconf, tstate, updClusters)
{
  tstate$objss <-
    if (length(tstate$objss) == 0)
      list(tstate$objs)
  else
    c(list(tstate$objs), tstate$objss[1:min(length(tstate$objss),tconf$obsHistDepth)])
  
  incompleteClusterIds <- 
    if (length(updClusters$cmpIds) > 0)
    {
      setdiff(
        updClusters$objs$id[sapply(updClusters$cmpIds, function(x) length(x) > 0 && length(x) < tconf$groupCount)],
        updClusters$unobsObjIds)
    }
  else
  {
    integer()
  }

  # Update history for changed cluster objects.
  if (length(incompleteClusterIds) > 0)
  {
      for (id in incompleteClusterIds)
      {
        newCmpIds <- unlist(updClusters$cmpIds[updClusters$objs$id == id])
        oldCmpIds <- unlist(tstate$cmpIds[tstate$objs$id == id])
        if (!identical(newCmpIds, oldCmpIds))
        {
          newCmpObs <- tstate$obs[tstate$obs$id %in% newCmpIds,]
          visNewCmpIds <- newCmpObs$id[!is.na(newCmpObs$x)]
          for (level in 1:length(tstate$objss))
          {
            updatedObj <- tstate$objss[[level]][tstate$objss[[level]]$id == id, tconf$ixytrInds]
            if (nrow(updatedObj) == 0)
            {
              break
            }
            tstate$objss[[level]][tstate$objss[[level]]$id == id, tconf$ixytrInds] <-
              updateClusterObj(tconf, updatedObj, visNewCmpIds, tstate$obss[[level]])
          }
        }
      }
  }
  return(tstate)
}