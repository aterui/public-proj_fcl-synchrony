pdf("figure1.pdf", width = 8, height = 4.5)
par(mfrow=c(1,2))
COL <- c("black", "red", "steelblue")

# Circle-shaped ecosystem
  ## draw circle
  theta <- seq(-pi, pi, length=100)
  plot(cos(theta), sin(theta), type="l", asp = 1, axes=F, ann=F, col=grey(0,0.5), lwd=2)
  
  ## random XY coordinate
  r <- 0.98
  N <- rpois(1,10*pi*r^2)
  Y <- runif(N, -r, r)
  xlim <- sqrt(r^2 - Y^2)
  X <- sapply(1:length(Y), function(x)runif(1, -xlim[x], xlim[x]))
  points(X, Y, pch = 21, col = NA, bg = grey(0,0.4))
  
  ## radius r
  segments(0, 0, 1, 0, col = grey(0,0.4), lwd = 1.5)
  text(0.5, 0.075, labels = "Radius"~italic("r"))
  
  ## separation distance x
  segments(X[1],Y[1], X[2], Y[2], lty = 2, col = grey(0,0.4), lwd = 1.5)
  
  ## Panel label
  legend("topleft", legend="(a)", bty = "n")
  
# Distance dependence in co-extinction probability
  x <- seq(0, 20, length = 100)
  beta <- c(10, 1, 0.1)
  Y <- sapply(beta, function(b)exp(-b*x) )
  matplot(x,Y, type = "l", axes = F, ann = F, col = COL, lty = 1, lwd = 2)
  axis(1);axis(2, las=2); box(bty="l")
  mtext("Distance"~italic(x), 1, line=3)
  mtext("Co-extinction probability"~~rho, 2, line=3)
  
  ## Panel label
  legend("topright", legend="(b)", bty = "n")

dev.off()
