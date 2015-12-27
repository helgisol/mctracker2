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
source('updateTstateForCompleteClusters.R')
source('calcIdGroups.R')
source('calcSeedCluster.R')
source('createSeeds.R')
source('coordinateClusters.R')
source('calcClusterCenWeights.R')

createEmptyTdata <- function()
{
  tconf1 <<- list(
    xyInds = c(2,3), # Point coordinate indices in observation data frame.
    ixytrInds = c(1, 2,3,4,5),
    dRdT = 4, # Radius growth time factor for time difference correction.
    tol = 1e-3, # Tolerance for mean shift process breaking.
    leaveClusterRFactor = 1.5, # Scale factor for cluster radius for leaving component treshold.
    groupCount = 3,
    typeIncompleteCluster = 1,
    typeNonconsistentCmp = 2,
    typeNewObs = 3)
  tstate1 <<- list()
  tstate1[[1]] <<- list(
    pts = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double(),g=integer()),
    cmps = list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double()),
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