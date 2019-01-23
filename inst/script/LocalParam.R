library(BiocParallel)
devtools::load_all()

## bpstart(p <- MulticoreParam(2))
## bpstop(p)
## res <- bplapply(1:2, sqrt, BPPARAM=MulticoreParam(2))

trace(BiocParallel:::.bpstop_nodes, browser)

bpstop(bpstart(LocalParam(2)))
p <- bpstart(LocalParam(2))
gc(); gc(); gc()

bpstart(p <- LocalParam(2))
bpstop(p)
res <- bplapply(1:2, sqrt, BPPARAM=LocalParam(2))
res <- bplapply(1:200, sqrt, BPPARAM=LocalParam(2))
