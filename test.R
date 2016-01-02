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
source('updateClusterCnt.R')
source('updateExistingClusters.R')
source('calcClusterCnt2.R')
source('updateCompleteClusters.R')
source('calcIdGroups.R')
source('calcSeedCluster.R')
source('createSeeds.R')
source('coordinateClusters.R')
source('calcClusterCenWeights.R')
source('updateObss.R')
source('updateClusterObj.R')
source('calcClusterTcWeights.R')
source('calcUpdCluster.R')
source('updateCoordClusters.R')

createEmptyTdata <- function()
{
  tconf1 <<- list(
    xyInds = c(2,3), # Point coordinate indices in observation data frame.
    ixytrInds = 1:5,
    ixytrgInds = 1:6,
    tftcInds = c(7,8),
    dRdT = 4, # Radius growth time factor for time difference correction.
    tol = 1e-3, # Tolerance for calculation of time difference between components' first times in a cluster.
    tolClusterTcDiffsMax = 1e-5, # Tolerance for mean shift process breaking.
    clusterRFactorReunion = 0.9, # Scale factor for cluster radius for re-union component treshold.
    clusterRFactorLost = 1.4, # Scale factor for cluster radius for leaving component treshold.
    clusterRFactorFirst = 1.1, # Scale factor for radius of the first point in cluster.
    obsHistDepth = 3, # Depth of stored observation history (at least 1).
    groupCount = 3,
    typeIncompleteCluster = 1,
    typeNonconsistentCmp = 2,
    typeNewObs = 3)
  tstate1 <<- list()
  tstate1[[1]] <<- list(
    obs = NULL, # Current observations' data table.
    obss = NULL, # Observations' history.
    cmpIds = list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double()),
    objss = NULL, # Cluster objects history.
    cmpIdsUpd = NULL,
    objsUpd = NULL,
    lastId = 0)
}

createObs <- function()
{
  obs1 <<- sampleObs()
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
    createObs()
  }
  processObs(iter)
  if (visualize)
  {
    visualizeClusters(tstate1[[iter+1]], iter)
  }
}