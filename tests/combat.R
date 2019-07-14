library(sva)
library(bladderbatch)
data(bladderdata)
source("../src/MyCombat.R")
dat <- bladderEset[1:50,]
pheno = pData(dat)
edata = exprs(dat)
batch = pheno$batch
mod = model.matrix(~ as.factor(cancer), data = pheno)
dat = edata
batch = batch
mod = NULL
par.prior = TRUE
prior.plots = T
mean.only = FALSE
BPPARAM = bpparam("SerialParam")

result<-combat(dat, batch, mod, par.prior, prior.plots, 
                  mean.only, ref.batch = NULL, BPPARAM = bpparam("SerialParam")) 