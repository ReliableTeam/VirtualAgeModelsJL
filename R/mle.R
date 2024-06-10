mle <- function(obj, ...) UseMethod("mle")

mle.vam <- function(vam, theta=NULL, data = data.frame(), datacov= data.frame()) {
    ## IMPORTANT: don't put jl(mle)(vam$model, data, datacov)
    ## because data is recognized as the jl(data) which corresponds to method 
    ## jlvalue(data) is the converted jlvalue object of the R object data 
    mle <- if (is.null(theta)) {
            jl(mle)(vam$model, jlvalue(data), jlvalue(datacov))
    } else {
            jl(mle)(vam$model, jlvalue(theta), jlvalue(data), jlvalue(datacov))
    }
    vam$mle <- mle
    params(vam)
}