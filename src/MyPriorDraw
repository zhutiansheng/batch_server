drawPrior<-function(priorData,batchName="2") {
  gamma.hat<-priorData$gamma.hat
  delta.hat<-priorData$delta.hat
  gamma.bar <- rowMeans(gamma.hat)
  t2 <- rowVars(gamma.hat)
  a.prior <- apply(delta.hat, 1, aprior)
  b.prior <- apply(delta.hat, 1, bprior)
  batchNum<-which(names(priorData$passTest)==batchName)
  
  par(mfrow = c(2, 2))
  lwd=2
  myColor = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  tmp <- density(gamma.hat[batchNum, ])
  plot(tmp, type = "l",bty="l",las=1,lwd=lwd, col=myColor[5], main = expression(paste("Density Plot of Batch for ",
                                                hat(gamma))))
  xx <- seq(min(tmp$x), max(tmp$x), length = 100)
  lines(xx, dnorm(xx, gamma.bar[batchNum], sqrt(t2[batchNum])),lwd=lwd,  col = myColor[6])
  qqnorm(gamma.hat[batchNum, ],bty="l",las=1, lwd=lwd, col=myColor[2],main = expression(paste("Normal Q-Q Plot of Batch for ",
                                                 hat(gamma))))
  qqline(gamma.hat[batchNum, ], lwd=lwd, col = myColor[6])
  tmp <- density(delta.hat[batchNum, ])
  xx <- seq(min(tmp$x), max(tmp$x), length = 100)
  tmp1 <- list(x = xx, y = dinvgamma(xx, a.prior[batchNum], b.prior[batchNum]))
  plot(tmp, type = "l",bty="l",las=1, ylim = c(0, max(tmp$y, tmp1$y)),lwd=lwd, col=myColor[5],
       main = expression(paste("Density Plot of Batch for ",
                               hat(delta))))
  lines(tmp1,lwd=lwd,  col = myColor[6])
  invgam <- 1/qgamma(1 - ppoints(ncol(delta.hat)), a.prior[batchNum],
                     b.prior[batchNum])
  qqplot(invgam, delta.hat[batchNum, ],bty="l",las=1, lwd=lwd, col = myColor[2], main = expression(paste("Inverse Gamma Q-Q Plot of Batch for ",
                                                         hat(delta))), ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")
  lines(c(0, max(invgam)), c(0, max(invgam)), lwd=lwd, col = myColor[6])
}

