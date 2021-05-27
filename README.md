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
Below the status of the automatic Travis CI tests on the master branch

[![Build Status](https://travis-ci.com/project-gen3sis/R-package.svg?branch=master)](https://travis-ci.com/project-gen3sis/R-package)

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

### Contribution process
The main branches of the gen3sis repo are:
*	master
*	development

The master branch should always reflect the state of the current release of gen3sis on CRAN. The development branch contains the working additions/changes to the code that are to be included in the next release.
You should not work on either of these branches directly. Rather, to make changes or work on a new feature, you should create a separate branch off the development branch. While working on your branch, frequently merge changes from development to stay up to date. Once your work is ready, and before you merge your branch into development, make sure to merge any changes from development and verify the code is compiling and tests are passing. Once these checks have been done, create a pull request to merge your branch into development. You can request reviewers for your pull request directly via GitHub. After your pull request is approved, or if it has not been reviewed within 30 days, it will be merged into development. 
The branch hotfix-development exists for small (one commit only) changes that are not worth creating a new branch for (for instance, small bugfixes, readme or help files edits, etc.). A pull request can then be created to merge those changes into development.
New features should never be merged directly into master. Only hotfixes to the current release may be merged into master. For hotfixes, create a separate branch from master, make the fix and verify it, and then merge the hotfix branch into master and development. Similarly to above, the hotfix-master branch exists for small (one commit only) bugfixes to the current release. A pull request can then be created to merge those changes into master and development.
The gen3sis workflow is inspired by the RevBayes workflow: https://revbayes.github.io/developer


