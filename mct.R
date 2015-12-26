mct <- function(tdata, obs)
{
  obsNewIds <- setdiff(obs$id, tdata$pts$id) # Vector of ID's for new points.
  obsDelIds <- setdiff(tdata$pts$id, obs$id) # Vector of ID's for deleted points.
  obsExiIds <- setdiff(obs$id, obsNewIds) # Vector of ID's for existed points.
  #obsHidIds <- obsExiIds[is.na(obsExiIds$x)] # Vector of ID's for hidden points.
  
  updateExistingClusters(tdata, obs)

  tol <- 1e-3 # Tolerance for mean shift process breaking.
  dRdT <- 4 # Radius growth time factor for time difference correction.
  d <- distMapCalc(obs, dRdT) # Distance map for all points. d[i,i] is filled, but distance for same group's point is NA.
  w <- clusterCenWeightMapCalc(obs, d, dRdT) # Calculate weight map for cluster center calculation.
  n <- nrow(obs) # Number of points.

  clusters <- clusterCalc(obs, d, w, n, tdata$xyInds, tol, dRdT)
  components <- lapply(clusters$inds, function(x) obs$id[x])
  objects <- data.frame(
    id = clusters$id,
    x = clusters$x,
    y = clusters$y,
    t = clusters$t);

  newTdata <- list(pts = obs, cmps = components, objs = objects, w = w, xyInds = tdata$xyInds)
  return(newTdata)
}