calcSeedHistObj <- function(
  tconf,
  tstate,
  incompleteClusterObjs,
  nonconsistentCmpObs,
  newObs)
{
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
  
  return(objs2)
}