## formula <- Time & Type ~ (ARAInf(0.6)| Weibull(1.0,3.0))

#' @export
vam <- function(form, data = data.frame(), datacov = data.frame()) {
    jl_code <- paste0("@vam(",paste(as.character(form)[-1], collapse=" ~ "), ")")
    vam <- list(
        jl = jl_code,
        model = jl(as.name(jl_code)),
        data = jlvalue(data),
        datacov = jlvalue(datacov)
    )
    class(vam) <- "vam"
    vam
}

#' @export
simulate.vam <- function(vam, n = 30) {
    toR(jl[[rand]](vam$model, jlvalue(as.integer(n))))
    ## or toR(jlcall("rand", vam$model, as_jlvalue(as.integer(n))))
}

