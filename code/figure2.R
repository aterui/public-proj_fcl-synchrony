source("function_FCLtheory.R")

# Parameter set ----
## Model parameters
  q <- c(0.1, 1) # establishment rate
  E <- c(0.1, 1) # extinction rate
  
  lambda <- 3 # patch density
  r <- seq(1, 10000, length = 15000) # ecosystem radius
  beta <- c(10, 1, 0.1) # rate parameter for co-extinction probability
  A <- c(1E+1, 1E+2, 1E+3) # dispersal range

## Figure parameters
  COL <- c("black", "red", "steelblue")
  para <- expand.grid(q,E)
  panel <- c("(a)", "(b)", "(c)", "(d)")
  lty <- c(1,2,4)

# Draw figure ----
pdf("figure2.pdf", width = 6.5, height = 6)
  par(mfrow = c(2,2), mar = c(3,3,2,1), oma = c(4,4,3,2))
  
  for(i in 1:nrow(para)){
    P <- panel[i]
    q <- c(para[i,1], rep(0.1, 2))
    E <- para[i,2]
    
    FCL <- as.matrix( sapply(beta, function(x) fclfun(lambda = lambda, A = A, q = q, E = E, r = r, beta = x)$fcl ) )
    matplot(log(r,10), FCL, type="n", ann = F, axes = F, ylim=c(0,3))
    for(i in 1:length(beta)){ lines(FCL[,i] ~ log(r,10), col = COL[i])}
    
    axis(1); axis(2, las=2); box(bty="l")
    lab <- substitute(var1~italic("q")[1]*" = "*var2*","~italic("e")*" = "*var3, list(var1 = P, var2 = q, var3 = E))
    mtext(lab)
  }  
  # Ylab
  mtext("Food chain length", 2, outer = T)
  # Xlab
  mtext(expression("Ecosystem size ("*log[10]~italic("r")*")"), 1, outer = T)
dev.off()
