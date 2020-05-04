![logo](logo.png)

# 

# General Engine for Eco-Evolutionary Simulations

This is the repository for the R-package of the Gen3sis engine.

Gen3sis is a spatially-explicit eco-evolutionary mechanistic model with a modular implementation. It allows exploring the consequences of ecological and macroevolutionary processes across realistic or theoretical spatio-temporal landscapes.

Gen3sis is licensed under a [GPLv3 License](https://www.gnu.org/licenses/gpl-3.0.html)) ETHZ 2020 <doi.org/10.5905/ethz-1007-251>

Package authorship according to: http://epub.wu.ac.at/3269/1/Report114.pdf

### How to run one simple simulation


```{r}
library("gen3sis")

#print package version
paste("GEN3SIS version:", packageVersion("gen3sis"))

#run simulation
run_simulation(config = "../simulations/config/user_defined_oskar/configs/config_1.R", input_directory = "../simulations/landscape/WorldCenter_4d")


#cat mood
cat("  xD  ")
```


