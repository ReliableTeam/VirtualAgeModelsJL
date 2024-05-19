## formula <- Time & Type ~ (ARAInf(0.6)| Weibull(1.0,3.0))

vam <- function(form, data = data.frame(), datacov = data.frame()) {
    jlcode <- paste0("@vam(",paste(as.character(form)[-1], collapse=" ~ "), ")")
    vam <- new.env() # could add later mle when needed
    vam$jl = jlcode
    vam$model = jl(as.name(jlcode))
    vam$data = jlvalue(data)
    vam$datacov = jlvalue(datacov)
    class(vam) <- "vam"
    vam
}

params <- function(obj, ...) UseMethod("params")
"params<-" <- function(obj, ...) UseMethod("params<-")

params.vam <- function(vam) jl[[params]](vam$model)
"params<-.vam" <- function(vam, pars) jl[["params!"]](vam$model, jlvalue(pars))
