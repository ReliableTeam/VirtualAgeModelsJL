rand.vam <- function(vam, n = 30) simulate.vam(vam, n)

simulate.vam <- function(vam, n = 30) {
    R(jl(rand)(vam$model, as.integer(n)))
}