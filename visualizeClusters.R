visualizeClusters <- function(tstate, domain, iter)
{
  if (nrow(tstate$obs) == 0)
  {
    return()
  }
  # Load library for circle drowing.
  library("plotrix")
  # Plot all observations as dots, colored by groups.
  plot(tstate$obs$x, tstate$obs$y, col=tstate$obs$g, xlim=domain$x, ylim=domain$y, pch=20,
         asp=1, xlab="x", ylab="y", main=paste("Iter #", iter))
  # Plot all observation IDs, colored by groups (at right).
  text(tstate$obs$x, tstate$obs$y, as.character(tstate$obs$id), pos=4, col=tstate$obs$g)
  # Plot component graph for each cluster.
  for(i in 1:length(tstate$cmpIds))
  { 
    xCen <- tstate$objs$x[i]
    yCen <- tstate$objs$y[i] 
    cmpIds <- tstate$cmpIds[[i]]
    for(id in cmpIds)
    {
      xPt <- tstate$obs$x[tstate$obs$id == id]
      yPt <- tstate$obs$y[tstate$obs$id == id]
      points(c(xCen, xPt), c(yCen, yPt), col=tstate$obs$g[tstate$obs$id == id], type="o")
    }
  }
  # Plot all cluster centers as rhombi.
  points(tstate$objs$x, tstate$objs$y, pch=23, col=max(tstate$obs$g)+1)
  # Plot all cluster IDs (at left).
  text(tstate$objs$x, tstate$objs$y, as.character(tstate$objs$id), pos=2, col=max(tstate$obs$g)+1)
  # Plot circles for clusters.
  draw.circle(tstate$objs$x, tstate$objs$y, tstate$obs$r[1], border=max(tstate$obs$g)+1)
}