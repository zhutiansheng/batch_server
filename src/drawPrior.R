
drawPrior<-function(priorData,batchName) {
  
  par(mfrow = c(2, 2))
  tmp <- density(gamma.hat[1, ])
  plot(tmp, type = "l", main = expression(paste("Density Plot of First Batch ",
                                                hat(gamma))))
  xx <- seq(min(tmp$x), max(tmp$x), length = 100)
  lines(xx, dnorm(xx, gamma.bar[1], sqrt(t2[1])), col = 2)
  qqnorm(gamma.hat[1, ], main = expression(paste("Normal Q-Q Plot of First Batch ",
                                                 hat(gamma))))
  qqline(gamma.hat[1, ], col = 2)
  tmp <- density(delta.hat[1, ])
  xx <- seq(min(tmp$x), max(tmp$x), length = 100)
  tmp1 <- list(x = xx, y = dinvgamma(xx, a.prior[1], b.prior[1]))
  plot(tmp, typ = "l", ylim = c(0, max(tmp$y, tmp1$y)),
       main = expression(paste("Density Plot of First Batch ",
                               hat(delta))))
  lines(tmp1, col = 2)
  invgam <- 1/qgamma(1 - ppoints(ncol(delta.hat)), a.prior[1],
                     b.prior[1])
  qqplot(invgam, delta.hat[1, ], main = expression(paste("Inverse Gamma Q-Q Plot of First Batch ",
                                                         hat(delta))), ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")
  lines(c(0, max(invgam)), c(0, max(invgam)), col = 2)
}