library(devEMF)
text.size <- 1.4
emf("figureS1.emf", width = 10, 5)
  par(mar=c(0,0,0,0), mfrow=c(1,2))
  plot(0, xlim=c(-0.2,1.2),ylim=c(-0.2,1.2), type="n", ann=F, axes=F)
  
  f1 <- function(x) x+0.2
  f2 <- function(x) x-0.2
  p1 <- seq(0,1,length=100)
  
  polygon(c(p1,rev(p1)), c(rep(0,100),rev(f1(p1))), col=grey(0,0.2), border=F)
  polygon(c(p1,rev(p1)), c(rep(0,100),rev(f2(p1))), col="white", border=F)
  polygon(c(0,2,2,0), c(1,1,2,2), col="white", border=F)
  polygon(c(1,2,2,1), c(0,0,2,2), col="white", border=F)
  segments(0,0,0,1)
  segments(0,0,1,0)
  
  text(-0.05, -0.05, label = ~italic("0"), cex = text.size)
  text(0.2, -0.05, label = ~italic("x"), cex = text.size)
  text(1, -0.05, label = ~italic("L"), cex = text.size)
  text(1.1, 0, label = ~italic("p")[1], cex = text.size)
  text(0, 1.1, label = ~italic("p")[2], cex = text.size)
  
  legend("topleft", legend = "(a)", bty = "n", cex = text.size)
  
  # Circle-shaped ecosystem
  ## draw circle
  theta <- seq(-pi, pi, length=100)
  plot(cos(theta), sin(theta), type="l", asp = 1, axes=F, ann=F, col=grey(0,0.5), lwd=2)
  abline(0.5,0)
  x <- sqrt(1 - 0.5^2)
  segments(0,0,x,0.5)
  segments(0,0,0,0.5, lty=2)
  
  text(0, 0.6, label=~italic("L"), cex = text.size)
  text(-0.1, 0.25, label=~xi, cex = text.size)
  text(0.25, 0.25, label=~italic("r"), cex = text.size)
  
  legend("topleft", legend = "(b)", bty = "n", cex = text.size)
dev.off()