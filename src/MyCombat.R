combat<-function (dat, batch, mod = NULL, par.prior = TRUE, prior.plots = FALSE, 
          mean.only = FALSE, ref.batch = NULL, BPPARAM = bpparam("SerialParam")) 
{
  ##inner function
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
  
    
  if (mean.only == TRUE) {
    message("Using the 'mean only' version of ComBat")
  }
  if (length(dim(batch)) > 1) {
    stop("This version of ComBat only allows one batch variable")
  }
  batch <- as.factor(batch)
  batchmod <- model.matrix(~-1 + batch)
  if (!is.null(ref.batch)) {
    if (!(ref.batch %in% levels(batch))) {
      stop("reference level ref.batch is not one of the levels of the batch variable")
    }
    cat("Using batch =", ref.batch, "as a reference batch (this batch won't change)\n")
    ref <- which(levels(as.factor(batch)) == ref.batch)
    batchmod[, ref] <- 1
  }
  else {
    ref <- NULL
  }
  message("Found", nlevels(batch), "batches")
  n.batch <- nlevels(batch)
  batches <- list()
  for (i in 1:n.batch) {
    batches[[i]] <- which(batch == levels(batch)[i])
  }
  n.batches <- sapply(batches, length)
  if (any(n.batches == 1)) {
    mean.only = TRUE
    message("Note: one batch has only one sample, setting mean.only=TRUE")
  }
  n.array <- sum(n.batches)
  design <- cbind(batchmod, mod)
  check <- apply(design, 2, function(x) all(x == 1))
  if (!is.null(ref)) {
    check[ref] <- FALSE
  }
  design <- as.matrix(design[, !check])
  message("Adjusting for", ncol(design) - ncol(batchmod), 
          "covariate(s) or covariate level(s)")
  if (qr(design)$rank < ncol(design)) {
    if (ncol(design) == (n.batch + 1)) {
      stop("The covariate is confounded with batch! Remove the covariate and rerun ComBat")
    }
    if (ncol(design) > (n.batch + 1)) {
      if ((qr(design[, -c(1:n.batch)])$rank < ncol(design[, 
                                                          -c(1:n.batch)]))) {
        stop("The covariates are confounded! Please remove one or more of the covariates so the design is not confounded")
      }
      else {
        stop("At least one covariate is confounded with batch! Please remove confounded covariates and rerun ComBat")
      }
    }
  }
  NAs <- any(is.na(dat))
  if (NAs) {
    message(c("Found", sum(is.na(dat)), "Missing Data Values"), 
            sep = " ")
  }
  cat("Standardizing Data across genes\n")
  if (!NAs) {
    B.hat <- solve(crossprod(design), tcrossprod(t(design), 
                                                 as.matrix(dat)))
  }
  else {
    B.hat <- apply(dat, 1, Beta.NA, design)
  }
  if (!is.null(ref.batch)) {
    grand.mean <- t(B.hat[ref, ])
  }
  else {
    grand.mean <- crossprod(n.batches/n.array, B.hat[1:n.batch, 
                                                     ])
  }
  if (!NAs) {
    if (!is.null(ref.batch)) {
      ref.dat <- dat[, batches[[ref]]]
      var.pooled <- ((ref.dat - t(design[batches[[ref]], 
                                         ] %*% B.hat))^2) %*% rep(1/n.batches[ref], n.batches[ref])
    }
    else {
      var.pooled <- ((dat - t(design %*% B.hat))^2) %*% 
        rep(1/n.array, n.array)
    }
  }
  else {
    if (!is.null(ref.batch)) {
      ref.dat <- dat[, batches[[ref]]]
      var.pooled <- rowVars(ref.dat - t(design[batches[[ref]], 
                                               ] %*% B.hat), na.rm = TRUE)
    }
    else {
      var.pooled <- rowVars(dat - t(design %*% B.hat), 
                            na.rm = TRUE)
    }
  }
  stand.mean <- t(grand.mean) %*% t(rep(1, n.array))
  if (!is.null(design)) {
    tmp <- design
    tmp[, c(1:n.batch)] <- 0
    stand.mean <- stand.mean + t(tmp %*% B.hat)
  }
  s.data <- (dat - stand.mean)/(sqrt(var.pooled) %*% t(rep(1, 
                                                           n.array)))
  message("Fitting L/S model and finding priors")
  batch.design <- design[, 1:n.batch]
  if (!NAs) {
    gamma.hat <- solve(crossprod(batch.design), tcrossprod(t(batch.design), 
                                                           as.matrix(s.data)))
  }
  else {
    gamma.hat <- apply(s.data, 1, Beta.NA, batch.design)
  }
  delta.hat <- NULL
  for (i in batches) {
    if (mean.only == TRUE) {
      delta.hat <- rbind(delta.hat, rep(1, nrow(s.data)))
    }
    else {
      delta.hat <- rbind(delta.hat, rowVars(s.data[, i], 
                                            na.rm = TRUE))
    }
  }
  gamma.bar <- rowMeans(gamma.hat)
  t2 <- rowVars(gamma.hat)
  a.prior <- apply(delta.hat, 1, aprior)
  b.prior <- apply(delta.hat, 1, bprior)
  if (prior.plots && par.prior) {
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
  gamma.star <- delta.star <- matrix(NA, nrow = n.batch, ncol = nrow(s.data))
  if (par.prior) {
    message("Finding parametric adjustments")
    results <- bplapply(1:n.batch, function(i) {
      if (mean.only) {
        gamma.star <- postmean(gamma.hat[i, ], gamma.bar[i], 
                               1, 1, t2[i])
        delta.star <- rep(1, nrow(s.data))
      }
      else {
        temp <- it.sol(s.data[, batches[[i]]], gamma.hat[i, 
                                                         ], delta.hat[i, ], gamma.bar[i], t2[i], a.prior[i], 
                       b.prior[i])
        gamma.star <- temp[1, ]
        delta.star <- temp[2, ]
      }
      list(gamma.star = gamma.star, delta.star = delta.star)
    }, BPPARAM = BPPARAM)
    for (i in 1:n.batch) {
      gamma.star[i, ] <- results[[i]]$gamma.star
      delta.star[i, ] <- results[[i]]$delta.star
    }
  }
  else {
    message("Finding nonparametric adjustments")
    results <- bplapply(1:n.batch, function(i) {
      if (mean.only) {
        delta.hat[i, ] = 1
      }
      temp <- int.eprior(as.matrix(s.data[, batches[[i]]]), 
                         gamma.hat[i, ], delta.hat[i, ])
      list(gamma.star = temp[1, ], delta.star = temp[2, 
                                                     ])
    }, BPPARAM = BPPARAM)
    for (i in 1:n.batch) {
      gamma.star[i, ] <- results[[i]]$gamma.star
      delta.star[i, ] <- results[[i]]$delta.star
    }
  }
  if (!is.null(ref.batch)) {
    gamma.star[ref, ] <- 0
    delta.star[ref, ] <- 1
  }
  message("Adjusting the Data\n")
  bayesdata <- s.data
  j <- 1
  for (i in batches) {
    bayesdata[, i] <- (bayesdata[, i] - t(batch.design[i, 
                                                       ] %*% gamma.star))/(sqrt(delta.star[j, ]) %*% t(rep(1, 
                                                                                                           n.batches[j])))
    j <- j + 1
  }
  bayesdata <- (bayesdata * (sqrt(var.pooled) %*% t(rep(1, 
                                                        n.array)))) + stand.mean
  if (!is.null(ref.batch)) {
    bayesdata[, batches[[ref]]] <- dat[, batches[[ref]]]
  }
  return(bayesdata)
}