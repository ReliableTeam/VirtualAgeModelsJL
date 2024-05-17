.onLoad <- function(lib, pkg) {
    if (!require(jl4R)) {
        stop("jl4R needs to be installed!")
    }
    jlusing("VirtualAgeModels")
}