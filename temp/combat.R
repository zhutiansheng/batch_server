library(sva)
library(fitdistrplus)
library(extraDistr)
set.seed(12345)
library(bladderbatch)
data(bladderdata)
source("src/MyCombat.R")
environment(combat) <- asNamespace('sva')
dat <- bladderEset[1:50,]
pheno = pData(dat)
edata = exprs(dat)
batch = pheno$batch
mod = model.matrix(~ as.factor(cancer), data = pheno)
dat = edata
batch = batch
#mod = NULL
par.prior = TRUE
prior.plots = T
mean.only = FALSE
BPPARAM = bpparam("SerialParam")

result<-combat(dat, batch, mod, par.prior="auto", fit.method="mle", 
                  mean.only, ref.batch = NULL, BPPARAM = bpparam("SerialParam")) 
res<-result$bayesdata
write.csv(res,"f:/abc.csv",row.names = T,quote = F,na="")
resul2<-ComBat(dat, batch, mod, par.prior=T, prior.plots=T, 
               mean.only, ref.batch = NULL, BPPARAM = bpparam("SerialParam")) 
dat[sample(1:50,10),sample(1:50,10)]<-NA
