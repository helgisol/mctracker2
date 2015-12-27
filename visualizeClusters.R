visualizeClusters <- function(tstate, iter)
{
  if (nrow(tstate$pts) == 0)
  {
    return()
  }
  # Load library for circle drowing.
  library("plotrix")
  # Plot all observations as dots, colored by groups.
  plot(tstate$pts$x, tstate$pts$y, col=tstate$pts$g, pch=20, asp=1, xlab="x", ylab="y", main=paste("Iter #", iter))
  # Plot all observation IDs, colored by groups (at right).
  text(tstate$pts$x, tstate$pts$y, as.character(tstate$pts$id), pos=4, col=tstate$pts$g)
  # Plot component graph for each cluster.
  for(i in 1:length(tstate$cmps))
  { 
    xCen <- tstate$objs$x[i]
    yCen <- tstate$objs$y[i] 
    cmp <- tstate$cmps[[i]]
    for(id in cmp)
    {
      xPt <- tstate$pts$x[tstate$pts$id == id]
      yPt <- tstate$pts$y[tstate$pts$id == id]
      points(c(xCen, xPt), c(yCen, yPt), col=tstate$pts$g[tstate$pts$id == id], type="o")
    }
  }
  # Plot all cluster centers as rhombi.
  points(tstate$objs$x, tstate$objs$y, pch=23, col=max(tstate$pts$g)+1)
  # Plot all cluster IDs (at left).
  text(tstate$objs$x, tstate$objs$y, as.character(tstate$objs$id), pos=2, col=max(tstate$pts$g)+1)
  # Plot circles for clusters.
  draw.circle(tstate$objs$x, tstate$objs$y, tstate$pts$r[1], border=max(tstate$pts$g)+1)
}