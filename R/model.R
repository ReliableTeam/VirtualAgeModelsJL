## formula <- Time & Type ~ (ARAInf(0.6)| Weibull(1.0,3.0))

vam <- function(form, data = data.frame(), datacov = data.frame()) {
    jlcode <- paste0("@vam(", deparse(rexpr2jlexpr(form)), ")")
    vam <- new.env() # could add later mle when needed
    vam$jl <- jlcode
    vam$model <- jlvalue_eval(jlcode)
    vam$data <- jlvalue(data)
    vam$datacov <- jlvalue(datacov)
    class(vam) <- "vam"
    update(vam, data=data, datacov=datacov)
    vam
}

params <- function(obj, ...) UseMethod("params")
"params<-" <- function(obj, ...) UseMethod("params<-")

params.vam <- function(vam) R(jl(params)(vam$model))
"params<-.vam" <- function(vam, pars) jl(`params!`)(vam$model, pars)

## To update data and/or datacov
update.vam <- function(vam, data = data.frame(), datacov = data.frame()) {
    if(nrow(data) > 0) vam$data <- data
    if(nrow(datacov) > 0) vam$datacov <- datacov
    jl(`data!`)(vam$model, vam$data, vam$datacov)
}

## To get data and/or datacov
model.frame.vam <- function(vam, datacov=TRUE) {
    list(
        data = jl(data)(vam$model)
    ) 
}