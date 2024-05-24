# VirtualAgeModelsJL

Calling [VirtualAgeModels.jl](https://) inside R

## Requirement

* Install [julia](https://julialang.org)
* Inside `julia`, install the package `VirtualAgeModels.jl`:
``` julia
Import Pkg; Pkg.add("VirtualAgeModels")
```
* Finally, `VirtualAgeModelsJL` is a wrapper of `VirtualAgeModels.jl` depending on [jl4R](https://github.com/rcqls/jl4R) to install before.

## Install inside `R`

Choose one of these two methods:

* Inside Bash Terminal:
``` bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/ReliableTeam/VirtualAgeModelsJL/HEAD/inst/install.sh)"
```
* Inside `R`:
``` R
remotes::install_github("ReliableTeam/VirtualAgeModelsJL")
```
