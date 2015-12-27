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
  
  seeds <- createSeeds(tconf, oldTstate, newTstate)
  seeds$d <- calcDistMap(tconf, seeds) # Distance map for all seeds.
  seeds$w <- calcClusterCenWeightMap(tconf, seeds) # Calculate weight map for cluster center calculation.

  newTstate <- updateTstateForCompleteClusters(tconf, oldTstate, newTstate)

  sproutClusters <- calcSproutClusters(tconf, seeds)
  clusters <- sproutClusters
  components <- lapply(clusters$inds, function(x) obs$id[x])
  objects <- data.frame(
    id = clusters$id,
    x = clusters$x,
    y = clusters$y,
    t = clusters$t,
    r = clusters$r);

  newTstate$cmps <- c(newTstate$cmps, components)
  newTstate$objs <- rbind(newTstate$objs, objects)

  return(newTstate)
}