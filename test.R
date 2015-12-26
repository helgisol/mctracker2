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
  tdata <<- list()
  tdata[[1]] <<- list(
    pts = data.frame(id=integer(),x=double(),y=double(),t=double(),r=double(),R=double(),g=integer()),
    cmps=list(),
    objs = data.frame(id=integer(),x=double(),y=double(),t=double()),
    w=matrix(double(),nrow=0,ncol=0))
}

createObs <- function()
{
  obs <<- sampleObs()
}

processObs <- function(iter)
{
  tdata[[iter+1]] <<- mct(tdata, obs[[iter]])
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
    visualizeClusters(tdata[[iter+1]])
  }
}