![gen3sis](./inst/logo/gen3sis_logo.png)


# General Engine for Eco-Evolutionary Simulations

This is the repository for the R-package of the gen3sis engine.

gen3sis is a spatially-explicit eco-evolutionary mechanistic model with a modular implementation. It allows exploring the consequences of ecological and macroevolutionary processes across realistic or theoretical spatio-temporal landscapes.

gen3sis is licensed under a [GPLv3 License](https://www.gnu.org/licenses/gpl-3.0.html) deriving from ETHZ 2020 <doi.org/10.5905/ethz-1007-251> and has package authorship according to: http://epub.wu.ac.at/3269/1/Report114.pdf

### The main methods are described here:
* O Hagen, B Flueck, F Fopp, JS Cabral, F Hartig, M Pontarp, TF Rangel, L Pellissier (2021) gen3sis: the general engine for eco-evolutionary simulations on the origins of biodiversity. bioRxiv. [doi:10.1101/2021.03.24.436109](https://doi.org/10.1101/2021.03.24.436109)


### How to install gen3sis

gen3sis is avabaile on [CRAN](https://CRAN.R-project.org/package=gen3sis). You can install the latest CRAN release via

```{r}
install.packages("gen3sis")
```

you can also install the latest development release from GitHub via 

```{r}
devtools::install_github(repo = "project-gen3sis/R-package/", 
  dependencies = TRUE, build_vignettes = TRUE)
```
Below the status of the automatic Git CI R-CMD-check tests

DEVELOPMENT [![R-CMD-check](https://github.com/project-gen3sis/R-package/actions/workflows/R-CMD-check.yaml/badge.svg?branch=development)](https://github.com/project-gen3sis/R-package/actions/workflows/R-CMD-check.yaml)

MASTER [![R-CMD-check](https://github.com/project-gen3sis/R-package/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/project-gen3sis/R-package/actions/workflows/R-CMD-check.yaml)
### How to run one simple simulation

Load and run a simulation with the desired config and landscapes. Exemple data is provided with the package
```{r}
library("gen3sis")

datapath <- system.file(file.path("extdata", "WorldCenter"), package = "gen3sis")

sim <- run_simulation(config = file.path(datapath, "config/config_worldcenter.R"), 
               landscape = file.path(datapath, "landscape"),
               output_directory = tempdir(),
               verbose=0)
```
A summary statistics is stored at 'sim' more data can be save using the oberver function

### How to visualize a simulation

Plot the summary statistics of a simulation

```{r}
plot_summary(sim)
```

### Check package version

Make sure you have the latest gen3sis version

```{r}
#print package version
paste("gen3sis version:", packageVersion("gen3sis"))
```
