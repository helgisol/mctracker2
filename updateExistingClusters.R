updateExistingClusters <- function(tdata, obs, xyInds)
{
  if (length(tdata$cmps) > 0)
  {
    for (i in 1:length(tdata$cmps))
    {
      cmps <- tdata$cmps[[i]]
      oldPts <- tdata$pts[tdata$pts$id %in% cmps,]
      newPts <- obs[obs$id %in% cmps, ]
      newHiddenPts <- newPts[is.na(newPts$x),]
      if (nrow(newHiddenPts) > 0)
      {
        tdata$pts[tdata$pts$id %in% newHiddenPts$id,] <- newHiddenPts
        updateClusterCnt(tdata, xyInds, i)
      }
    }
  }
}