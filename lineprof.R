library(lineprof)
library(microbenchmark)
source("Benchmarking.R")
l <- lineprof(LoadData())
l
shine(l)

