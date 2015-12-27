calcClusterCenWeights <- function(
  tconf,
  obs,
  i,
  j)
{
  ws <- obs$r[i] / (obs$r[j] + tconf$dRdT * abs(obs$t[j] - obs$t[i]))
  return(ws)
}