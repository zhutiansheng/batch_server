##inner function
dinvgamma<-function(x, shape, rate = 1/scale, scale = 1) 
{
  stopifnot(shape > 0)
  stopifnot(rate > 0)
  ifelse(x <= 0, 0, ((rate^shape)/gamma(shape)) * x^(-shape - 
                                                       1) * exp(-rate/x))
}
bprior<-function(gamma.hat) 
{
  m <- mean(gamma.hat)
  s2 <- var(gamma.hat)
  (m * s2 + m^3)/s2
}
aprior<-function (gamma.hat) 
{
  m <- mean(gamma.hat)
  s2 <- var(gamma.hat)
  (2 * s2 + m^2)/s2
}
it.sol<-function (sdat, g.hat, d.hat, g.bar, t2, a, b, conv = 1e-04) 
{
  n <- rowSums(!is.na(sdat))
  g.old <- g.hat
  d.old <- d.hat
  change <- 1
  count <- 0
  while (change > conv) {
    g.new <- postmean(g.hat, g.bar, n, d.old, t2)
    sum2 <- rowSums((sdat - g.new %*% t(rep(1, ncol(sdat))))^2, 
                    na.rm = TRUE)
    d.new <- postvar(sum2, n, a, b)
    change <- max(abs(g.new - g.old)/g.old, abs(d.new - d.old)/d.old)
    g.old <- g.new
    d.old <- d.new
    count <- count + 1
  }
  adjust <- rbind(g.new, d.new)
  rownames(adjust) <- c("g.star", "d.star")
  adjust
}
postmean<-function (g.hat, g.bar, n, d.star, t2) 
{
  (t2 * n * g.hat + d.star * g.bar)/(t2 * n + d.star)
}
postvar<-function (sum2, n, a, b) 
{
  (0.5 * sum2 + b)/(n/2 + a - 1)
}