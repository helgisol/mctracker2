isSeedClusterDetachedCalc <- function(seedClusters, inds)
{
  n <- length(inds)
  if (n == 1)
  {
    return(TRUE)
  }
  allInds <- seedClusters$inds[inds]
  if (length(unique(unlist(lapply(allInds, length)))) > 1)
  {
    return(FALSE)
  }
  firstInds <- allInds[[1]]
  for(i in 2:length(allInds))
  {
    if (!all(firstInds == allInds[[i]]))
    {
      return(FALSE)
    }
  }
  return(TRUE)
}