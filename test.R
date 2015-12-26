#testMct(1)
#testMct(2)

source('mct.R')
source('sampleObs.R')
source('distMapCalc.R')
source('clusterCenWeightMapCalc.R')
source('distCalc.R')
source('initSeedClustersCalc.R')
source('seedClusterCalc.R')
source('clusterCenCalc.R')
source('conflictingRankCalc.R')
source('isSeedClusterDetachableCalc.R')
source('detachPoint.R')
source('clusterCalc.R')
source('visualizeClusters.R')
source('updateClusterCnt.R')
source('updateExistingClusters.R')
source('calcClusterCnt.R')
source('updateTstateForCompleteClusters.R')
source('calcIdGroups.R')

createEmptyTdata <- function()
{
  tconf1 <<- list(
    xyInds = c(2,3), # Point coordinate indices in observation data frame.
    dRdT = 4, # Radius growth time factor for time difference correction.
    tol = 1e-3, # Tolerance for mean shift process breaking.
    leaveClusterRFactor = 1.5, # Scale factor for cluster radius for leaving component treshold.
    groupCount = 3)
  tstate1 <<- list()
  tstate1[[1]] <<- list(
    pts = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double(),g=integer()),
    cmps = list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double()),
    n = 0,
    d = matrix(double(),nrow=0,ncol=0),
    w = matrix(double(),nrow=0,ncol=0))
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