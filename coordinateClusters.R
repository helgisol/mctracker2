coordinateClusters <- function(tconf, oldTstate, seeds, sproutClusters, lastId)
{
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
  coordClusters$cmps <- lapply(sproutClusters$inds, function(x) seeds$objs$id[x])
  for (i in 1:length(sproutClusters$id))
  {
    cmpInds <- sproutClusters$inds[[i]]
    types <- seeds$objs$type[cmpInds]
    if (tconf$typeIncompleteCluster %in% types)
    {
      clusterObjs <- seeds$objs[cmpInds,]
      cmpIds <- clusterObjs$id[clusterObjs$type == tconf$typeIncompleteCluster]
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
      if (seeds$objs$type[cmpInd] == tconf$typeIncompleteCluster)
      {
        oldTstateInd <- which(oldTstate$objs$id == seeds$objs$id[cmpInd])
        realCmpIds <- oldTstate$cmps[[oldTstateInd]]
        cmpIds <- c(cmpIds, realCmpIds)
      }
      else
      {
        cmpIds <- c(cmpIds, seeds$objs$id[cmpInd])
      }
    }
    coordClusters$cmps[[i]] <- cmpIds
  }
  return(coordClusters)
}