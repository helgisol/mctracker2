#testMct(1)
#testMct(2)

source('mct.R')
source('sampleObs.R')
source('calcDistMap.R')
source('calcClusterCenWeightMap.R')
source('calcPointDist.R')
source('calcSeedClusters.R')
source('calcClusterCnt.R')
source('calcConflictingRank.R')
source('calcIsSeedClusterDetachable.R')
source('detachPoint.R')
source('calcSproutClusters.R')
source('visualizeClusters.R')
source('updateCompleteClusters.R')
source('calcSeedCluster.R')
source('createSeeds.R')
source('coordinateClusters.R')
source('calcClusterCenWeights.R')
source('updateObss.R')
source('updateClusterObj.R')
source('calcClusterTcWeights.R')
source('calcUpdCluster.R')
source('updateCoordClusters.R')
source('calcUpdClusters.R')
source('updateObjss.R')
source('updateSeedCluster.R')
source('addSproutCluster.R')
source('calcIsIntersected.R')
source('calcIsSubsetOf.R')
source('updateSeedClustersCen.R')
source('calcSeedDist.R')
source('calcClusterCmpDist.R')
source('calcDistWeightMap.R')
source('calcSeedHistObj.R')
source('calcObsDist.R')

createEmptyTdata <- function()
{
  tconf1 <<- list(
    xyInds = 2:3, # Point coordinate indices in observation data frame.
    xytInds = 2:4,
    ixytrInds = 1:5,
    ixytrgInds = 1:6,
    tftcInds = 7:8,
    cenInds = 2:3, # Seed cluster center coordinate indices.
    dRdT = 4, # Radius growth time factor for time difference correction.
    tol = 1e-3, # Tolerance for calculation of time difference between components' first times in a cluster.
    tolClusterTcDiffsMax = 1e-5, # Tolerance for mean shift process breaking.
    clusterRFactorLost = 1.4, # Scale factor for cluster radius for leaving component treshold.
    clusterRFactorTcMax = 1.08, # Maximal scale factor for radius of the oldest component in cluster.
    obsHistDepth = 3, # Depth of stored observation history (at least 1).
    groupCount = 3,
    distWeightScaleFactor = 0.33, # Factor for seeed distance weight calculation (scale for change).
    sSortDistFactor = matrix(c(1.0,1.05,1.2, 1.05,1.0,1.08, 1.2,1.08,1.0),3,3), # Seed sort distance factor.
    maxTimeHistDist = 0.3, # Maximal time distance for extraction from history for seed distance calculation.
    sSort = list(incompleteCluster=1, nonconsistentCmp=2, newObs=3), # Seed sorts.
    scStatus = list(free=1, cond=2, quaziFree=3, detached=4, freeOrQuazi=c(1,3))) # Seed cluster statuses.
  tstate1 <<- list()
  tstate1[[1]] <<- list(
    obs = NULL, # Current observations' data table.
    obss = NULL, # Observations' history.
    cmpIds = list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double()),
    objss = NULL, # Cluster objects history.
    lastId = 0)
}

createTestObs <- function()
{
  obsInfo <- sampleObs2()
  obs1 <<- obsInfo$obs
  domain1 <<- obsInfo$domain
}

processObs <- function(iter)
{
  tstate1[[iter+1]] <<- mct(tconf1, tstate1[[iter]], obs1[[iter]])
}

testMct <- function(iter = 1, visualize = TRUE)
{
  if (iter == 1)
  {
    createEmptyTdata()
    createTestObs()
  }
  processObs(iter)
  if (visualize)
  {
    visualizeClusters(tstate1[[iter+1]], domain1, iter)
  }
}