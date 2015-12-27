updateClusterCnt <- function(tdata, ind)
{
  #newClusterCen <- calcClusterCnt2(tdata$pts, tdata$xyInds, tdata$w, ind)
  
  cmps <- tdata$cmps[[ind]]
  cmps <- cmps[!is.na(tdata$pts$x[tdata$pts$id %in% cmps])]
  if (length(cmps) == 0)
  {
    tdata$objs$x[ind] <- NA
    return
  }
  ptsInds <- which(tdata$pts$id %in% cmps)
  ts <- tdata$pts$t[ptsInds]
  refPtInd <- ptsInds[which(ts == max(ts))][1]
  clusterPts <- tdata$pts[ptsInds, tdata$xyInds]
  clusterWs <- tdata$w[refPtInd, ptsInds]
  newClusterCen <- calcClusterCnt(clusterPts, clusterWs)
  tdata$objs$x <- newClusterCen$x
  tdata$objs$y <- newClusterCen$y
  tdata$objs$t <- max(ts)
  tdata$objs$r <- max(tdata$pts$r[ptsInds]) # !!!! Must be changed !!!!
}