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

createEmptyTdata <- function()
{
  tdata1 <<- list()
  tdata1[[1]] <<- list(
    pts = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double(),R=double(),g=integer()),
    cmps =list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double()),
    w = matrix(double(),nrow=0,ncol=0),
    xyInds = c(2,3)) # Point coordinate indices in observation data frame.
}

createObs <- function()
{
  obs1 <<- sampleObs()
}

processObs <- function(iter)
{
  tdata1[[iter+1]] <<- mct(tdata1[[iter]], obs1[[iter]])
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
    visualizeClusters(tdata1[[iter+1]])
  }
}