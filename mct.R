mct <- function(tconf, oldTstate, obs)
{
  #obsNewIds <- setdiff(obs$id, oldTstate$pts$id) # Vector of ID's for new points.
  #obsDelIds <- setdiff(oldTstate$pts$id, obs$id) # Vector of ID's for deleted points.
  #obsExiIds <- setdiff(obs$id, obsNewIds) # Vector of ID's for existed points.
  #obsHidIds <- obsExiIds[is.na(obsExiIds$x)] # Vector of ID's for hidden points.

  newTstate <- list(
    cmps = list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double()))
  newTstate$pts <- obs
  newTstate$n <- nrow(obs) # Number of points.
  
  seeds <- createSeeds(tconf, oldTstate, newTstate)
  newTstate <- updateTstateForCompleteClusters(tconf, oldTstate, newTstate)

  newTstate$d <- calcDistMap(tconf, seeds) # Distance map for all points. d[i,i] is filled, but distance for same group's point is NA.
  newTstate$w <- calcClusterCenWeightMap(tconf, seeds, newTstate$d) # Calculate weight map for cluster center calculation.

  clusters <- calcClusters(tconf, seeds, newTstate$d, newTstate$w, newTstate$n)
  components <- lapply(clusters$inds, function(x) obs$id[x])
  objects <- data.frame(
    id = clusters$id,
    x = clusters$x,
    y = clusters$y,
    t = clusters$t,
    r = clusters$r);

  newTstate$cmps <- components
  newTstate$objs <- objects
  
  return(newTstate)
}