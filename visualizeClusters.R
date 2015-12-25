visualizeClusters <- function(tdata = tdata2)
{
  #library(ggplot2)
  library("plotrix")
  #qplot(x, y, data = obs[[1]], color = as.factor(g))
  plot(tdata$pts$x, tdata$pts$y, col=tdata$pts$g, pch=20, asp=1, xlab="x", ylab="y")
  text(tdata$pts$x, tdata$pts$y, as.character(tdata$pts$id), pos=4, col=tdata$pts$g)
  points(tdata$objs$x, tdata$objs$y, pch=23, col=max(tdata$pts$g)+1)
  text(tdata$objs$x, tdata$objs$y, as.character(tdata$objs$id), pos=2, col=max(tdata$pts$g)+1)
  draw.circle(tdata$objs$x, tdata$objs$y, tdata$pts$r[1], border=max(tdata$pts$g)+1)
  for(i in 1:length(tdata$cmps))
  { 
    xCen <- tdata$objs$x[i]
    yCen <- tdata$objs$y[i] 
    cmp <- tdata$cmps[[i]]
    for(id in cmp)
    {
      xPt <- tdata$pts$x[tdata$pts$id == id]
      yPt <- tdata$pts$y[tdata$pts$id == id]
      points(c(xCen, xPt), c(yCen, yPt), col=tdata$pts$g[tdata$pts$id == id], type="o")
    }
  }
}