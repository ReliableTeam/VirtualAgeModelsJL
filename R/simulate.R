rand.vam <- function(vam, n = 30) simulate.vam(vam, n)

simulate.vam <- function(vam, n = 30) {
    toR(jl[[rand]](vam$model, jlvalue(as.integer(n))))
    ## or toR(jlcall("rand", vam$model, as_jlvalue(as.integer(n))))
}