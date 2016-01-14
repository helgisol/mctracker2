sampleObs <- function() # Sample of observation data for mc-tracker.
{
  domain <- data.frame(
    x=c(-0.1, 7.4),
    y=c(-0.55, 0.55))
  
  obs1 <- data.frame(
    id=(1:21),
    x=c(0.0, 1.0, 1.9, 2.8, 3.7, 4.6, 5.5, 0.5, 1.5, 2.4, 3.3, 4.2, 5.1, 6.0, 0.6, 1.6, 2.5, 3.4, 4.3, 5.2, 6.1),
    y=c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1, -0.1),
    t=c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
    r=c(0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55),
    g=c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3))
  obs1$g = as.integer(obs1$g)

  obs2 <- obs1
  obs2$x <- obs2$x + 0.03
  obs2$y <- obs2$y - 0.04
  obs2$t <- obs2$t + 0.1
  obs2$r <- obs2$r + 0.01
  obs2$x[obs2$id==1] <- NA
  obs2$x[obs2$id==16] <- NA
  obs2 <- rbind(obs2, list(id=22, x=6.2, y=0.0, t=0.1, r=0.55, g=1))
  
  obs3 <- obs2
  obs3$x <- obs3$x - 0.01
  obs3$y <- obs3$y + 0.02
  obs3$t <- obs3$t + 0.1
  obs3$r <- obs3$r - 0.01
  obs3 <- rbind(obs3, list(id=23, x=6.7, y=0.1, t=0.3, r=0.55, g=2))
  obs3 <- rbind(obs3, list(id=24, x=6.8, y=-0.2, t=0.3, r=0.55, g=3))
  obs3 <- obs3[obs3$id != 1,]

  obs4 <- obs3
  obs4$x[obs4$id==16] <- 1.7
  obs4$x <- obs4$x + 0.05
  obs4$y <- obs4$y - 0.06
  obs4$t <- obs4$t + 0.2
  obs4$r <- obs4$r - 0.01

  obs <- list(obs1, obs2, obs3, obs4)
  return(list(obs=obs,domain=domain))
}

sampleObs2 <- function() # Simlple walking through 3 cameras.
{
  domain <- data.frame(
    x=c(-1.6, 1.6),
    y=c(-1.6, 1.6))
  
  obs <- list()
  obs[[1]] <- data.frame(
    id=c(1),
    x=c(1.0),
    y=c(0.0),
    t=c(0.0),
    r=c(0.5),
    g=c(1))
  obs[[2]] <- data.frame(
    id=c(1),
    x=c(0.71),
    y=c(0.71),
    t=c(0.1),
    r=c(0.5),
    g=c(1))
  obs[[3]] <- data.frame(
    id=c(1, 2),
    x=c(-0.1, 0.1),
    y=c(0.9, 1.1),
    t=c(0.2, 0.15),
    r=c(0.5, 0.6),
    g=c(1, 2))
  obs[[4]] <- data.frame(
    id=c(1, 2),
    x=c(NA, -0.71),
    y=c(NA, 0.71),
    t=c(0.25, 0.3),
    r=c(0.5, 0.6),
    g=c(1, 2))
  obs[[5]] <- data.frame(
    id=c(1, 2, 3),
    x=c(NA, -1.1, -0.9),
    y=c(NA, -0.1, 0.1),
    t=c(0.4, 0.45, 0.35),
    r=c(0.5, 0.6, 0.4),
    g=c(1, 2, 3))
  obs[[6]] <- data.frame(
    id=c(1, 2, 3),
    x=c(NA, NA, -0.71),
    y=c(NA, NA, -0.71),
    t=c(0.5, 0.5, 0.5),
    r=c(0.5, 0.6, 0.4),
    g=c(1, 2, 3))
  obs[[7]] <- data.frame(
    id=c(1, 2, 3),
    x=c(NA, NA, 0.1),
    y=c(NA, NA, -1.1),
    t=c(0.55, 0.6, 0.6),
    r=c(0.5, 0.6, 0.4),
    g=c(1, 2, 3))
  obs[[8]] <- data.frame(
    id=c(1, 2, 3),
    x=c(-0.1, NA, 0.81),
    y=c(-0.9, NA, -0.61),
    t=c(0.61, 0.7, 0.7),
    r=c(0.5, 0.6, 0.4),
    g=c(1, 2, 3))
  obs[[9]] <- data.frame(
    id=c(1, 2, 3),
    x=c(0.61, NA, NA),
    y=c(-0.81, NA, NA),
    t=c(0.71, 0.8, 0.8),
    r=c(0.5, 0.6, 0.4),
    g=c(1, 2, 3))
  obs[[10]] <- data.frame(
    id=c(1, 2, 3),
    x=c(1.0, NA, NA),
    y=c(0.0, NA, NA),
    t=c(0.8, 0.86, 0.86),
    r=c(0.5, 0.6, 0.4),
    g=c(1, 2, 3))
  return(list(obs=obs,domain=domain))
}
