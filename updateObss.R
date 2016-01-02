# 1. Update observations' list for current stage.
# 2. Update history of observations (obss) and history of cluster objects (objss).
# 3. Update tf (time of the first detection) & tc (time of the first inclsion in cluster) for all observations.
updateObss <- function(tconf, tstate, obs)
{
  prevObs <- NULL
  if (!is.null(tstate$obs)) # Update tstate$obss & tstate$objss.
  {
    prevObs <- tstate$obs # Remomber the previous observations for future processing.
    tstate$obss <-
      if (length(tstate$obss) == 0)
        list(tstate$obs[,tconf$ixytrgInds])
      else
        c(list(tstate$obs[,tconf$ixytrgInds]), tstate$obss[1:min(length(tstate$obss),tconf$obsHistDepth)])
    tstate$objss <-
      if (length(tstate$objss) == 0)
        list(tstate$objs)
      else
        c(list(tstate$objs), tstate$objss[1:min(length(tstate$objss),tconf$obsHistDepth)])
  }
  tstate$obs <- obs # Update tstate$obs by new observations.
  tstate$obs$tf <- tstate$obs$t # Set tf (time of the first detection) by default values.
  tstate$obs$tc <- rep(NA, nrow(tstate$obs)) # Set tc (time of the first inclsion in cluster) by default values.
  if (!is.null(prevObs)) # Update tf & tc for existing observations in tstate$obs.
  {
    ids <- intersect(prevObs$id, obs$id) # Get IDs for existing observations.
    tstate$obs[tstate$obs$id %in% ids, tconf$tftcInds] <- prevObs[prevObs$id %in% ids, tconf$tftcInds]
  }
  return(tstate)
}