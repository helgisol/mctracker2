# Calculate euclidian distance between two observations.
calcObsDist <- function(tconf, pi, pj, ti, tj, ri, rj)
{
  ptDists <- calcPointDist(pi, pj)
  ptDists - ri - rj - tconf$dRdT * abs(tj - ti)
}