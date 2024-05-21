## formula <- Time & Type ~ (ARAInf(0.6)| Weibull(1.0,3.0))

vam <- function(form, data = data.frame(), datacov = data.frame()) {
    jlcode <- paste0("@vam(", deparse(rexpr2jlexpr(form)), ")")
    vam <- new.env() # could add later mle when needed
    vam$jl = jlcode
    vam$model = jl(as.name(jlcode))
    vam$data = jlvalue(data)
    vam$datacov = jlvalue(datacov)
    jl[["data!"]](vam$model, vam$data, vam$datacov)
    class(vam) <- "vam"
    vam
}

params <- function(obj, ...) UseMethod("params")
"params<-" <- function(obj, ...) UseMethod("params<-")

params.vam <- function(vam) jl[[params]](vam$model)
"params<-.vam" <- function(vam, pars) jl[["params!"]](vam$model, jlvalue(pars))

model.frame.vam <- function(vam, datacov=TRUE) {
    list(
        data = jl[[data]](vam$model)
    ) 
}