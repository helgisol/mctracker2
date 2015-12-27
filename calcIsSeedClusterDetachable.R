calcIsSeedClusterDetachable <- function(seedClusters, inds)
{
  n <- length(inds)
  if (n == 1)
  {
    return(TRUE)
  }
  if (any(seedClusters$cRank[inds] != 0.0))
  {
    return(FALSE)
  }
  allInds <- seedClusters$inds[inds]
  firstInds <- allInds[[1]]
  for(i in 2:length(allInds))
  {
    if (!identical(firstInds, allInds[[i]]))
    {
      return(FALSE)
    }
  }
  return(TRUE)
}