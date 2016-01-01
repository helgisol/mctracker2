updateObss <- function(tconf, tstate, obs)
{
  prevObs <- NULL
  if (!is.null(tstate$obs))
  {
    prevObs <- tstate$obs
    tstate$obss <- ifelse(
      length(tstate$obss) == 0,
      list(tstate$obs[,tconf$ixytrgInds]),
      list(tstate$obs[,tconf$ixytrgInds], tstate$obss[1:min(length(tstate$obss),tconf$obsHistDepth)]))
    tstate$objss <- ifelse(
      length(tstate$objss) == 0,
      list(tstate$objs),
      list(tstate$objs, tstate$objss[1:min(length(tstate$objss),tconf$obsHistDepth)]))
  }
  tstate$obs <- obs
  tstate$obs$tf <- tstate$obs$t # Time of the first detection.
  tstate$obs$tc <- rep(NA, nrow(tstate$obs)) # Time of the first inclsion into the corresponding cluster.
  if (!is.null(prevObs))
  {
    ids <- intersect(prevObs$id, obs$id)
    tstate$obs[tstate$obs$id %in% ids, tftcInds] <- prevObs[prevObs$id %in% ids, tftcInds]
  }
  return(tstate)
}