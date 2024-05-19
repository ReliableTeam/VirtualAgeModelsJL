mle <- function(obj, ...) UseMethod("mle")

mle.vam <- function(vam, theta=NULL, data = data.frame(), datacov= data.frame()) {
    mle <- if (is.null(theta)) {
            jl[[mle]](vam$model, jlvalue(data), jlvalue(datacov))
    } else {
            jl[[mle]](vam$model, jlvalue(theta), jlvalue(data), jlvalue(datacov))
    }
    vam$mle <- mle
    params(vam)
}