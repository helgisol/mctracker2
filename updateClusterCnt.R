updateClusterCnt <- function(tconf, tstate, ind)
{
  #newClusterCen <- calcClusterCnt2(tstate$pts, tstate$xyInds, tstate$w, ind)
  
  cmps <- tstate$cmps[[ind]]
  cmps <- cmps[!is.na(tstate$pts$x[tstate$pts$id %in% cmps])]
  if (length(cmps) == 0)
  {
    tstate$objs$x[ind] <- NA
    return
  }
  ptsInds <- which(tstate$pts$id %in% cmps)
  ts <- tstate$pts$t[ptsInds]
  refPtInd <- ptsInds[which(ts == max(ts))][1]
  clusterPts <- tstate$pts[ptsInds, tconf$xyInds]
  #clusterWs <- tstate$w[refPtInd, ptsInds]
  clusterWs <- calcClusterCenWeights(tconf, tstate$pts, refPtInd, ptsInds)
  newClusterCen <- calcClusterCnt(clusterPts, clusterWs)
  tstate$objs$x[ind] <- newClusterCen$x
  tstate$objs$y[ind] <- newClusterCen$y
  tstate$objs$t[ind] <- max(ts)
  tstate$objs$r[ind] <- max(tstate$pts$r[ptsInds]) # !!!! Must be changed !!!!
  return(tstate)
}