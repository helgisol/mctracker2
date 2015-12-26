visualizeClusters <- function(tstate, iter)
{
  if (nrow(tstate$pts) == 0)
  {
    return()
  }
  #library(ggplot2)
  library("plotrix")
  #qplot(x, y, data = obs[[1]], color = as.factor(g))
  plot(tstate$pts$x, tstate$pts$y, col=tstate$pts$g, pch=20, asp=1, xlab="x", ylab="y", main=paste("Iter #", iter))
  text(tstate$pts$x, tstate$pts$y, as.character(tstate$pts$id), pos=4, col=tstate$pts$g)
  points(tstate$objs$x, tstate$objs$y, pch=23, col=max(tstate$pts$g)+1)
  text(tstate$objs$x, tstate$objs$y, as.character(tstate$objs$id), pos=2, col=max(tstate$pts$g)+1)
  draw.circle(tstate$objs$x, tstate$objs$y, tstate$pts$r[1], border=max(tstate$pts$g)+1)
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
}