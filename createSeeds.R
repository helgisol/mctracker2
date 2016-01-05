createSeeds <- function(tconf, tstate, updClusters)
{
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
  incompleteClusterObjs <- updClusters$objs[updClusters$objs$id %in% incompleteClusterIds, tconf$ixytrInds]
  newObsIds <- setdiff(tstate$obs$id, tstate$obss[[1]]$id) # Vector of ID's for pure new (previously non-existed) points.
  nonconsistentCmpObs <- tstate$obs[tstate$obs$id %in% updClusters$ncCmpIds, tconf$ixytrInds]
  newObs <- tstate$obs[tstate$obs$id %in% newObsIds,]
  
  objs <- rbind(
    cbind(
      incompleteClusterObjs,
      s = rep(tconf$sSort$incompleteCluster, nrow(incompleteClusterObjs))),
    cbind(
      nonconsistentCmpObs,
      s = rep(tconf$sSort$nonconsistentCmp, nrow(nonconsistentCmpObs))),
    cbind(
      newObs[,tconf$ixytrInds],
      s = rep(tconf$sSort$newObs, nrow(newObs))))

  seeds <- list(
    g = c(
      lapply(updClusters$cmpIds[updClusters$objs$id %in% incompleteClusterIds],
             function(x) tstate$obs$g[tstate$obs$id %in% x]),
      as.list(nonconsistentCmpObs$g),
      as.list(newObs$g)),
    objs = objs)
  
  seeds$d <- calcDistMap(tconf, seeds) # Distance map for all seeds.
  seeds$w <- calcClusterCenWeightMap(tconf, seeds) # Calculate weight map for cluster center calculation.
  
  if (length(tstate$objss) > 0)
  {
    objsOld <- do.call(rbind, tstate$objss)
    objsOld <- objsOld[objsOld$id %in% incompleteClusterObjs$id,]
    objs2 <- incompleteClusterObjs
    if (nrow(objs2) > 0)
    {
      for (i in 1:nrow(objs2))
      {
        objs2Row <- objs2[i,]
        id <- objs2Row$id
        t <- objs2Row$t
        tt <- objsOld$t[objsOld$id == id]
        t2 <- min(tt[abs(tt-t) <= tconf$maxTimeHistDist])
        if (t2 < t)
        {
          objs2[i,] <- objsOld[objsOld$id == id & objsOld$t == t2,]
        }
        else
        {
          objs2[i,] <- list(id=id,x=NA,y=NA,t=NA,r=NA)
        }
      }
    }
    incompleteClusterObjs2 <- objs2
  }
  else
  {
    naVect <- rep(NA, nrow(incompleteClusterObjs))
    incompleteClusterObjs2 <- data.frame(id=incompleteClusterObjs$id,x=naVect,y=naVect,t=naVect,r=naVect)
  }
  
  if (length(tstate$obss) > 0)
  {
    obsOld <- do.call(rbind, tstate$obss)
    obsOld <- obsOld[obsOld$id %in% nonconsistentCmpObs$id,]
    obs2 <- nonconsistentCmpObs
    if (nrow(obs2) > 0)
    {
      for (i in 1:nrow(obs2))
      {
        obs2Row <- obs2[i,]
        id <- obs2Row$id
        t <- obs2Row$t
        tt <- obsOld$t[obsOld$id == id]
        t2 <- min(tt[abs(tt-t) <= tconf$maxTimeHistDist])
        if (t2 < t)
        {
          obs2[i,] <- obsOld[obsOld$id == id & obsOld$t == t2,]
        }
        else
        {
          obs2[i,] <- list(id=id,x=NA,y=NA,t=NA,r=NA)
        }
      }
    }
    nonconsistentCmpObs2 <- obs2
  }
  else
  {
    naVect <- rep(NA, nrow(nonconsistentCmpObs))
    nonconsistentCmpObs2 <- data.frame(id=nonconsistentCmpObs$id,x=naVect,y=naVect,t=naVect,r=naVect)
  }
  
  naVect <- rep(NA, nrow(newObs))
  newObs2 <- data.frame(id=newObs$id,x=naVect,y=naVect,t=naVect,r=naVect)
 
  objs2 <- rbind(
    cbind(
      incompleteClusterObjs2,
      s = rep(tconf$sSort$incompleteCluster, nrow(incompleteClusterObjs2))),
    cbind(
      nonconsistentCmpObs2,
      s = rep(tconf$sSort$nonconsistentCmp, nrow(nonconsistentCmpObs2))),
    cbind(
      newObs2,
      s = rep(tconf$sSort$newObs, nrow(newObs2))))
  
  seeds$objs2 <- objs2
   
  return(seeds)
}