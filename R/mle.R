mle <- function(obj, ...) UseMethod("mle")

mle.vam <- function(vam, theta=NULL, data = data.frame(), datacov= data.frame()) {
    mle <- if (is.null(theta)) {
            jl(mle)(vam$model, data, datacov)
    } else {
            jl(mle)(vam$model, theta, data, datacov)
    }
    vam$mle <- mle
    params(vam)
}