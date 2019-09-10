library(GEOquery)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("GEOquery")
BiocManager::install("proBatch")
BiocManager::install("GEOquery")

library(BatchQC)
#dataset 1
data(example_batchqc_data)
batch <- batch_indicator$V1
condition <- batch_indicator$V2

#dataset 2
data(bladderdata)
pheno <- pData(bladderEset)
edata <- exprs(bladderEset)
batch <- pheno$batch  
condition <- pheno$cancer

#data set 3
data(protein_example_data)
