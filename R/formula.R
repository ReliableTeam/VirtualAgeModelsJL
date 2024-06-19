## TODO: 
## remplacer vecteur v de longueur > 1 
## par jlstring(jl(v)))
# parse_vam_formula <- function(formula) {
#   if(formula[[1]] != as.name("~")) stop("Argument has to be a formula")
#   if(length(formula) == 2) { ## No response
#     response <- NULL
#     cm <- formula[[2]]
#   } else { ## with response
#     tmp <- formula[[2]]
#     ## simplify parenthesis
#     while(tmp[[1]] == as.name("(")) tmp <- tmp[[2]]
#     if(tmp[[1]] != as.name("&") && length(tmp) != 3) 
#             stop("Left part of formula of the form 'Time & Type'!")
#     if(length(tmp[[2]]) == 3 && tmp[[2]][[1]] == as.name("&")) {
#       response <- c(
#                 as.character(tmp[[2]][[2]]),
#                 as.character(tmp[[2]][[3]]),
#                 as.character(tmp[[3]])
#             )
#     } else response <- c(as.character(tmp[[2]]),as.character(tmp[[3]]))
#     cm <- formula[[3]]
#   }
#   ## simplify parenthesis
#   while(cm[[1]] == as.name("(")) cm <- cm[[2]]
#   pms <- list()
#   policy <- NULL
#   if(there.is.pm <- (cm[[1]] == as.name("&"))) { # there is a PM part
#     pm <- cm[[3]]
#     cm <- cm[[2]]
#     # deal with PM part
#     if(pm[[1]] == as.name("(")) {
#       pm <- pm[[2]]
#       if(pm[[1]] != as.name("|")) {
#         ## Case: No maintenance policy
#         #stop("Need a policy to manage Preventive Maintenance")
#         policy <- NULL
#       } else {
#         policy <- pm[[3]]
#         if(policy[[1]] == as.name("*")) {
#           ## Case: Composition of maintenance policies
#           # recursive function to detect maintenance policies
#           run.over.policies<-function(p) {
#             if(p[[1]] == as.name("*")) {
#               run.over.policies(p[[2]])
#               run.over.policies(p[[3]])
#             } else if(is.name(p[[1]])) {
#               p[[1]] <- as.name(paste0(as.character(p[[1]]), ".maintenance.policy"))
#               policies <<- c(policies,list(p))
#             }
#           }
#           ## init policies and
#           policies <- list()
#           run.over.policies(policy)
#           ## print(policies)
#           policy <- policies ##[[1]]
#         } else if(is.name(policy[[1]])) {
#           ## Case: One maintenance policy
#           policy[[1]] <- as.name(paste0(as.character(policy[[1]]),".maintenance.policy"))
#         }

#         # PMs
#         pm <- pm[[2]]
#       }
#       # parser for pm
#       parse_pm <- function(pm) {
#         if(is.name(pm[[1]])) {
#           pm[[1]] <- as.name(paste0(as.character(pm[[1]]),".va.model"))
#         }
#         pm
#       }
#       cpt.pms <- 0
#       while(pm[[1]] == as.name("+") ) {
#         if(length(pm) == 3) {
#           pms[[cpt.pms <- cpt.pms + 1]] <- parse.pm(pm[[3]])
#           pm <- pm[[2]]
#         }
#       }
#       pms[[cpt.pms <- cpt.pms + 1]] <- parse.pm(pm)
#     } else stop("Need parenthesis around the Preventive Maintenance terms")
#   }
#   # deal with CM PART
#   cms <- list()

#   # parser for cm
#   parse_cm <- function(cm) {
#     # print(there.is.pm)
#     # print(cm)
#     if(there.is.pm) {
#       if(cm[[1]] == as.name("(")) cm <- cm[[2]]
#       else stop("CM needs a family!")
#     }
#     if(cm[[1]] != as.name("|")) stop("CM needs a family!")
#     family <- cm[[3]]
#     if(is.name(family[[1]])) {
#       family[[1]] <- as.name(paste0(as.character(family[[1]]),".family.cm"))
#     }
#     cm <- cm[[2]]
#     if(is.name(cm[[1]])) {
#       cm[[1]] <- as.name(paste0(as.character(cm[[1]]),".va.model"))
#     }
#     list(model=cm,family=family)
#   }
#   cpt.cms <- 0
#   while( cm[[1]] == as.name("+") ) {
#     if(length(cm) == 3) {
#       cms[[cpt.cms <- cpt.cms + 1]] <- parse.cm(cm[[3]])
#       cm <- cm[[2]]
#     }
#   }
#   cms[[cpt.cms <- cpt.cms + 1]] <- parse.cm(cm)

#   ## Parse covariates
#   parse_covariates <- function(expr) {
#     form <- list()
#     params <- c()
#     add_term <- function(term,sign) {
#       ##print(term)
#       if(term[[1]]==as.name("*")) {
#         form <<- c(as.character(term[[3]]),form)
#         if(sign == as.name("-")) {
#            if(!(is.numeric(eval(term[[2]])))) stop("Only + is admitted between covariates definition in Bayes case.") else param_expr <- eval_vam(parse(text=paste0(sign,as.character(eval(term[[2]])))))
#         } else param_expr <- as.vector(eval_vam(term[[2]]))
#         params <<- c(param_expr, params)
#       }
#       ##print(list(form=form,params=params))
#     }
#     while(expr[[1]]==as.name("+") || expr[[1]]==as.name("-")) {
#       add_term(expr[[3]],as.character(expr[[1]]))
#       expr <- expr[[2]]
#     }
#     add_term(expr,"")
#     list(formula=eval(parse(text=paste0("~",paste(form,collapse="+")))),params=params)
#   }

#   convert_family <- function(fam) {
#     # eval_vam is here to evaluate the value if it is a symbol!
#     # We want to detect if there is covariates or not. Normally the operator | delimitating covariates has priority, expect possibly in Baysian case.
#     if(has.covariates <- (length(fam[[length(fam)]])>1 && fam[[length(fam)]][[1]] == as.name("|"))) {
#       covariates_expr <- fam[[length(fam)]][[3]]
#       fam[[length(fam)]] <- fam[[length(fam)]][[2]] # first argument of last terms becomes last argument of family
#     } else {
#     #In Bayesian case the ~ operator of the description of the prior distribution of the last argument of the family can possibly has priority on the operator | delimitating covariates
#       if(length(fam[[length(fam)]])>1 && fam[[length(fam)]][[1]] == as.name("~") && length(fam[[length(fam)]][[2]])>1 && fam[[length(fam)]][[2]][[1]] == as.name("|")){
#         has.covariates <- TRUE
#         covariates_expr <- fam[[length(fam)]][[2]][[3]]
#         fam[[length(fam)]][[2]] <- fam[[length(fam)]][[2]][[2]]
#       }
#     }

#     res<-list(
#         name=as.character(fam[[1]]),
#         params=sapply(fam[-1],function(e) as.vector(eval_vam(e)))
#         ## instead of : params=sapply(cm$family[-1],as.vector)
#         ## which does not work with negative real since element of tmp[-1] interpreted as call!
#     )
#     if(has.covariates) {
#       res$covariates <- parse.covariates(covariates_expr)
#     }
#     return(res)
#   }
#   convert_pm <- function(pm) {
#     n_pip<-c()
#     if(length(pm)>1){
#       for(i in 2:length(pm)){
#         if((length(pm[[i]])==3)&&(pm[[i]][[1]]==as.name("|"))) {
#           n_pip<-c(n_pip,i)
#         }
#       }
#     }
#     if(length(n_pip)==0) {
#       list(
#         name=as.character(pm[[1]]),
#         params=as.vector(if(length(pm)==1) numeric(0) else sapply(pm[2:length(pm)],function(e) as.vector(eval_vam(e))))
#       )
#     } else if(length(n_pip)==1) {
#       if(n_pip<(length(pm)-1)) {
#         stop("Maximum two arguments after a | in a maintenance effect!")
#       } else if(n_pip == length(pm)) {
#         if( typeof(tryCatch( as.double(eval_vam(pm[[length(pm)]][[3]])) ,error=function(e){FALSE},finally=function(e){TRUE}))!="logical"){
#             if((round(eval_vam(pm[[length(pm)]][[3]])) != eval_vam(pm[[length(pm)]][[3]]))||(round(eval_vam(pm[[length(pm)]][[3]]))<=0)) {
#               stop("Memory argument of a maintenance model has to be a strictly positive integer!")
#             } else {
#                 list(
#                   name=as.character(pm[[1]]),
#                   params=as.vector(if(length(pm)==2) as.vector(eval_vam(pm[[2]][[2]])) else c(sapply(pm[2:(length(pm)-1)],function(e) as.vector(eval_vam(e))),as.vector(eval_vam(pm[[length(pm)]][[2]])))),
#                   m=as.integer(eval_vam(pm[[length(pm)]][[3]]))
#                 )
#             }
#           } else {
#             list(
#               name=as.character(pm[[1]]),
#               params=as.vector(if(length(pm)==2) as.vector(eval_vam(pm[[2]][[2]])) else c(sapply(pm[2:(length(pm)-1)],function(e) as.vector(eval_vam(e))),as.vector(eval_vam(pm[[length(pm)]][[2]])))),
#               extra=as.character(pm[[length(pm)]][[3]])
#             )
#           }
#       } else {
#         if( typeof(tryCatch( as.double(eval_vam(pm[[length(pm)-1]][[3]])) ,error=function(e){FALSE},finally=function(e){TRUE}))!="logical"){
#           if((round(eval_vam(pm[[length(pm)-1]][[3]]))!=eval_vam(pm[[length(pm)-1]][[3]]))||(round(eval_vam(pm[[length(pm)-1]][[3]]))<0)) {
#             stop("Memory argument of a maintenance model has to be a positive integer!")
#           } else {
#               list(
#                 name=as.character(pm[[1]]),
#                 params=as.vector(if(length(pm)==3) as.vector(eval_vam(pm[[2]][[2]])) else c(sapply(pm[2:(length(pm)-2)],function(e) as.vector(eval_vam(e))),as.vector(eval_vam(pm[[length(pm)-1]][[2]])))),
#                 m=as.integer(eval_vam(pm[[length(pm)-1]][[3]])),
#                 extra=as.character(pm[[length(pm)]])
#               )
#           }
#         } else {
#           if( typeof(tryCatch( as.double(eval_vam(pm[[length(pm)]])) ,error=function(e){FALSE},finally=function(e){TRUE}))=="logical"){
#             stop("At least one of the two argument of maintenance model after a | must be a memory that is to say a non negative positive integer!")
#           } else {
#             if((round(eval_vam(pm[[length(pm)]]))!=eval_vam(pm[[length(pm)]]))||(round(eval_vam(pm[[length(pm)]]))<0)) {
#                 stop("Memory argument of a maintenance model has to be a positive integer!")
#               } else {
#                   list(
#                 name=as.character(pm[[1]]),
#                 params=as.vector(if(length(pm)==3) as.vector(eval_vam(pm[[2]][[2]])) else c(sapply(pm[2:(length(pm)-2)],function(e) as.vector(eval_vam(e))),as.vector(eval_vam(pm[[length(pm)-1]][[2]])))),
#                 m=as.integer(eval_vam(pm[[length(pm)]])),
#                 extra=as.character(pm[[length(pm) - 1]][[3]])
#                 )
#                 }
#           }

#         }
#       }
#     } else {
#       stop("Maximum one | in a maintenance effect!")
#     }
#   }
#   convert_mp <- function(mp) {#maintenance policy
#     if(is.null(mp)) list(name="None")
#     else if(is.list(mp)) {
#       list(name="MaintenancePolicyList",policies=lapply(mp,convert_mp))
#     }
#     else {

#       ## The function defining the maintenance policy
#       ## (registered in maintenance-policy-register.R or in any other R file)
#       mp.fct <- eval_vam(mp[[1]])
#       ## params used in the call mp
#       pars <- as.list(match.call(mp.fct,mp))[-1]

#       ## Default values are then automatically completed using declaration of maintenance policy
#       pars.default <- (as.list(mp.fct)->tmp)[-length(tmp)]
#       pars.default <- pars.default[sapply(pars.default,function(e) nchar(as.character(e)))!=0]
#       for(e in names(pars.default)) if(is.null(pars[[e]])) pars[[e]] <- pars.default[[e]]

#       ##print(list(pars=pars))

#       ## deal with model parameter which has a specific treatment
#     #   mod <- NULL
#     #   if(!is.null(pars[["model"]])) {
#     #     mod <- rcpp(eval_vam(pars[["model"]]))
#     #     pars[["model"]] <- NULL
#     #   }

#       res <- list(
#         name=as.character(mp[[1]]),
#         params=lapply(pars,eval_vam)
#       )
#     #   res[["with.model"]] <- !is.null(mod)
#     #   if(!is.null(mod)) res[["model"]] <- mod
#       res
#     }
#   }


#   res<-list(
#     response=response,
#     models=c(list(convert_pm(cms[[1]]$model)),lapply(pms[rev(seq(pms))],convert_pm)),
#     family=convert_family(cms[[1]]$family),
#     pm.policy=convert_mp(policy)
#   )
#   ## covariates direct acces
#   res$covariates <- res$family$covariates
#   res$family$covariates <- NULL

#   res$max_memory <- max(1,unlist(sapply(res$models,function(e) e$m)),na.rm=TRUE)
#   res

# }

# # use substitute coef in vam formula
# substitute_vam_formula <- function(formula,coef,model) {
#   if(missing(model)) model <- parse.vam.formula(formula)
#   if(missing(coef)) {
#     coef <- c(model$family$params,sapply(model$models,function(m) m$params))
#     if(!is.null(model$covariates)) coef <- c(coef,model$covariates$params)
#   }
#   if(!is.null(model$covariates)) {
#       nb_paramsCovariates <- length(model$covariates$params)
#     } else {
#       nb_paramsCovariates <- 0
#   }

#   coef<-unlist(coef)

#   nb_paramsFamily <- length(model$family$params)
#   nb_paramsCM <- length(model$models[[1]]$params)
#   nb_paramsPM <- sapply(model$models[-1],function(m) length(m$params))
#   form <- paste0(
#             paste(model$response,collapse=" & "),
#             "~ (",
#               strsplit(model$models[[1]]$name,"\\.")[[1]][1],
#               "(",
#               if(nb_paramsCM>0) paste(coef[nb_paramsFamily+(1:nb_paramsCM)],collapse=",") else "",
#               if(!is.null(model$models[[1]]$m) || !is.null(model$models[[1]]$extra)) {
#                 extra <- c()
#                 if(!is.null(model$models[[1]]$extra)) extra <- c(extra,model$models[[1]]$extra)
#                 if(!is.null(model$models[[1]]$m)) extra <- c(extra,model$models[[1]]$m)
#                 paste0("|",paste(extra,collapse=","))
#               } else "",
#               ")",
#             "|",
#               strsplit(model$family$name,"\\.")[[1]][1],
#               "(",
#                paste(coef[1:nb_paramsFamily],collapse=","),
#                if(!is.null(model$covariates)) {
#                  # unlist(nb_paramsPM) since nb_paramsPM is list() when no PM
#                  tmp<-coef[nb_paramsFamily + nb_paramsCM + sum(unlist(nb_paramsPM)) + (1:nb_paramsCovariates)]
#                  if(is.list(tmp)){##Bayesian case
#                     paste0("| (",
#                      paste(tmp,all.vars(model$covariates$formula),sep=") * ",collapse=" + (")
#                    )
#                  } else {
#                     tmp[tmp<0] <- paste0("(",tmp[tmp<0],")")
#                     paste0("|",
#                       paste(tmp,all.vars(model$covariates$formula),sep=" * ",collapse=" + ")
#                     )
#                  }
#                } else "",
#               ")",
#             ")"
#           )
#   if(length(model$models)>1) {
#     pms <- model$models[-1]
#     form <- paste0(form,
#               " & (",
#               paste(
#                 sapply(seq(pms),function(i) {
#                   paste0(
#                     strsplit(pms[[i]]$name,"\\.")[[1]][1],
#                     "(",
#                     if(nb_paramsPM[i]>0) paste(coef[nb_paramsFamily+nb_paramsCM+ifelse(i>1,sum(nb_paramsPM[1:(i-1)]),0)+(1:nb_paramsPM[i])],collapse=",") else "",
#                     if(!is.null(pms[[i]]$m) || !is.null(pms[[i]]$extra)) {
#                       extra <- c()
#                       if(!is.null(pms[[i]]$extra)) extra <- c(extra,pms[[i]]$extra)
#                       if(!is.null(pms[[i]]$m)) extra <- c(extra,pms[[i]]$m)
#                       paste0("|",paste(extra,collapse=","))
#                     } else "",
#                     ")"
#                   )
#                 }),
#                 collapse=" + "
#               ),
#               ")"
#             )
#   }
#   form <- eval(parse(text=form),envir=globalenv())
#   form
# }

# priors.from.vam.formula <- function(model) {
#   flatten.params <- c(model$family$params,unlist(sapply(model$models,function(e) e$params)))
#   ##if(!is.null(model$family$covariates)) flatten.params <- c(flatten.params,mode$family$covariates$params) ##Strange
#   if(!is.null(model$covariates)) flatten.params <- c(flatten.params,model$covariates$params)
#   if(all(sapply(flatten.params,class) == "formula") ) {
#     prior.families <- c("B","Beta","U","Unif","G","Gamma","Norm","N","NonInform","NInf","LNorm","LogNorm","LN")
#     ## clear "|" expression
#     flatten.params <- lapply(flatten.params,function(e) if(!as.character(e[[2]][[1]]) %in% prior.families) e[[2]][[2]] else e[[2]])
#     ## transform to list
#     parse.prior <- function(prior) {
#         ## declare here all the priors
#         Beta <- B <- Be <- function(a,b) list(name="Beta.prior",params=c(a,b))
#         Gamma <- G <- function(a,s) list(name="Gamma.prior",params=c(a,s))
#         Unif <- U <- function(a=0,b=1) list(name="Unif.prior",params=c(a,b))
#         Norm <- N <- function(m=0,s=1) list(name="Norm.prior",params=c(m,s))
#         LNorm <- LogNorm <- LN <- function(m=0,s=1) list(name="LNorm.prior",params=c(m,s))
#         NonInform <- NInf <- NI <- function(init=1,init_sigma=1) list(name="NonInform.prior",params=c(init,init_sigma))
#         res <- eval(prior) ## TODO or NOT TODO: eval(prior,parent.frame())
#         class(res) <- res$name #to be accessible as a class in R
#         res
#     }
#     flatten.params <- lapply(flatten.params,parse.prior)
#     return(flatten.params)
#   } else {
#     warning("Not a formula for bayesian.vam object!")
#     NULL # means not ok!
#   }
# }