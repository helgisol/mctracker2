coordinateClusters <- function(tconf, tstate, seeds, sproutClusters)
{
  lastId <- tstate$lastId
  if (length(sproutClusters$id) == 0)
  {
    return(sproutClusters)
  }
  coordClusters <- list()
  coordClusters$objs <- data.frame(
    id = sproutClusters$id,
    x = sproutClusters$x,
    y = sproutClusters$y,
    t = sproutClusters$t,
    r = sproutClusters$r)
  coordClusters$cmpIds <- lapply(sproutClusters$inds, function(x) seeds$objs$id[x])
  for (i in 1:length(sproutClusters$id))
  {
    cmpInds <- sproutClusters$inds[[i]]
    sSorts <- seeds$objs$s[cmpInds]
    if (tconf$sSort$incompleteCluster %in% sSorts)
    {
      clusterObjs <- seeds$objs[cmpInds,]
      cmpIds <- clusterObjs$id[clusterObjs$s == tconf$sSort$incompleteCluster]
      coordClusters$objs$id[i] <- min(cmpIds)
    }
    else
    {
      lastId <- lastId + 1
      coordClusters$objs$id[i] <- lastId
    }
    cmpIds <- integer()
    for (cmpInd in cmpInds)
    {
      if (seeds$objs$s[cmpInd] == tconf$sSort$incompleteCluster)
      {
        oldTstateInd <- which(tstate$objs$id == seeds$objs$id[cmpInd])
        realCmpIds <- tstate$cmpIds[[oldTstateInd]]
        cmpIds <- c(cmpIds, realCmpIds)
      }
      else
      {
        cmpIds <- c(cmpIds, seeds$objs$id[cmpInd])
      }
    }
    coordClusters$cmpIds[[i]] <- cmpIds
  }
  coordClusters$lastId <- lastId
  return(coordClusters)
}