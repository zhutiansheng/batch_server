# ##inner function
# dinvgamma<-function(x, shape, rate = 1/scale, scale = 1) 
# {
#   stopifnot(shape > 0)
#   stopifnot(rate > 0)
#   ifelse(x <= 0, 0, ((rate^shape)/gamma(shape)) * x^(-shape - 
#                                                        1) * exp(-rate/x))
# }
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

int.eprior<-function (sdat, g.hat, d.hat) 
{
  g.star <- d.star <- NULL
  r <- nrow(sdat)
  for (i in 1:r) {
    g <- g.hat[-i]
    d <- d.hat[-i]
    x <- sdat[i, !is.na(sdat[i, ])]
    n <- length(x)
    j <- numeric(n) + 1
    dat <- matrix(as.numeric(x), length(g), n, byrow = TRUE)
    resid2 <- (dat - g)^2
    sum2 <- resid2 %*% j
    LH <- 1/(2 * pi * d)^(n/2) * exp(-sum2/(2 * d))
    LH[LH == "NaN"] = 0
    g.star <- c(g.star, sum(g * LH)/sum(LH))
    d.star <- c(d.star, sum(d * LH)/sum(LH))
  }
  adjust <- rbind(g.star, d.star)
  rownames(adjust) <- c("g.star", "d.star")
  adjust
}
