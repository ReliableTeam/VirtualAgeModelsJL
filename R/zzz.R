.onLoad <- function(lib, pkg) {
    if (require(Rulia)) {
        ## cat("Rulia installed...\n")
    } else {
        stop("Rulia needs to be installed! Copy-paste in some bash terminal: /bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/rcqls/Rulia/HEAD/inst/install.sh)\" to install it.")
    }
    if(R(jlpkgisinstalled(VirtualAgeModels))) {
        Rulia:::jlusing_force("VirtualAgeModels")
        ## cat("VirtualAgeModels.jl installed...\n")
    } else {
        cat("Julia package VirtualAgeModels.jl is needed...\n")
        jlpkgadd(VirtualAgeModels)
        cat("Julia package VirtualAgeModels.jl installed\n")
        ## jlpkgadd(url="https://github.com/ReliableTeam/VirtualAgeModels.jl")
        ## stop("Julia package VirtualAgeModels.jl needs to be installed...")
    }
}