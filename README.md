![gen3sis](./inst/logo/gen3sis_logo.png)


# General Engine for Eco-Evolutionary Simulations

This is the repository for the R-package of the gen3sis engine [project-gen3sis git](https://github.com/project-gen3sis/R-package).

gen3sis is a spatially-explicit eco-evolutionary mechanistic model with a modular implementation. It allows exploring the consequences of ecological and macroevolutionary processes across realistic or theoretical spatio-temporal landscapes.

gen3sis is licensed under a [GPLv3 License](https://www.gnu.org/licenses/gpl-3.0.html) deriving from ETHZ 2020 <doi.org/10.5905/ethz-1007-251> and has package authorship according to: http://epub.wu.ac.at/3269/1/Report114.pdf

### How to cite
* O Hagen, B Flueck, F Fopp, JS Cabral, F Hartig, M Pontarp, TF Rangel, L Pellissier (2021) gen3sis: A general engine for eco-evolutionary simulations of the processes that shape Earth’s biodiversity. PLOS Biology. [doi:10.1371/journal.pbio.3001340](https://doi.org/10.1371/journal.pbio.3001340)


### How to install

gen3sis is avabaile on [CRAN](https://CRAN.R-project.org/package=gen3sis). You can install the latest CRAN release via

```{r}
install.packages("gen3sis")
```

you can also install the latest development release from GitHub via 

```{r}
devtools::install_github(repo = "project-gen3sis/R-package", 
  dependencies = TRUE, build_vignettes = TRUE)
```
Below the status of the automatic CI R-CMD-check tests

DEVELOPMENT [![R-CMD-check](https://github.com/project-gen3sis/R-package/actions/workflows/R-CMD-check.yaml/badge.svg?branch=development)](https://github.com/project-gen3sis/R-package/actions/workflows/R-CMD-check.yaml)

MASTER [![R-CMD-check](https://github.com/project-gen3sis/R-package/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/project-gen3sis/R-package/actions/workflows/R-CMD-check.yaml)
### How to use

#### Run one simulation

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

####  Visualize a simulation

Plot the summary statistics of a simulation
```{r}
plot_summary(sim)
```

#### Check installed version

Make sure you have the latest gen3sis version
```{r}
#print package version
paste("gen3sis version:", packageVersion("gen3sis"))
```

### How to contribute

In short, the main branches of the gen3sis repo are:
* **master** – reflects the current CRAN release. Only hotfixes or release-ready changes are merged here, typically just before CRAN submission.
* **development** – serves as the main working branch. All new features, improvements, and fixes should be merged here from separate feature or bugfix branches.

Great that you are contributing! For guidelines on contributing to this project, please refer to the [CONTRIBUTING.md](./CONTRIBUTING.md) file.

### Credits
We thank the developers of the following methods and dependencies:

- **Rcpp** Dirk Eddelbuettel and James Joseph Balamuta (2018). Extending R with C++: A Brief Introduction to Rcpp. The American Statistician. 72(1). URL https://doi.org/10.1080/00031305.2017.1375990.

- **BH** Dirk Eddelbuettel, John W. Emerson and Michael J. Kane (2021). BH: Boost C++ Header Files. R package. https://CRAN.R-project.org/package=BH

- **Matrix** Douglas Bates and Martin Maechler (2019). Matrix: Sparse and Dense Matrix Classes and Methods. R package. https://CRAN.R-project.org/package=Matrix

- **raster** Robert J. Hijmans (2021). raster: Geographic Data Analysis and Modeling. R package. https://CRAN.R-project.org/package=raster

- **gdistance** van Etten, J. (2017). R package gdistance: Distances and routes on geographical grids. Journal of Statistical Software, 76(1), 1–21. https://doi.org/10.18637/jss.v076.i13

- **sp** Roger S. Bivand, Edzer Pebesma, Virgilio Gomez-Rubio, 2013. Applied spatial data analysis with R, Second edition. Springer, NY. https://asdar-book.org/

- **stringr** Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package. https://CRAN.R-project.org/package=stringr

- **testthat** Hadley Wickham (2011). testthat: Get Started with Testing. The R Journal, vol. 3, no. 1, pp. 5--10, https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

- **formatR** Yihui Xie (2021). formatR: Format R Code Automatically. R package. https://CRAN.R-project.org/package=formatR

- **scico** Crameri, F. (2018). Scientific colour maps. Zenodo. http://doi.org/10.5281/zenodo.1243862 & Crameri, F., G.E. Shephard, and P.J. Heron (2020). The misuse of colour in science communication, Nature Communications, 11, 5444. doi:10.1038/s41467-020-19160-7
