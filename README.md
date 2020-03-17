![logo](logo.png)

# GENƎSIS

## General Engine for Eco-Evolutionary Simulations

This is the repository for the R-package of the Gen3sis engine.
Gen3sis is licensed under a [GPLv3 License](httpstldrlegal.comlicensegnu-general-public-license-v3-(gpl-3)) ETHZ 2020 <doi.org/10.5905/ethz-1007-251>

Package authorship according to: http://epub.wu.ac.at/3269/1/Report114.pdf

### How to run one simple simulation

library("gen3sis")

#print package version
paste("GEN3SIS version:", packageVersion("gen3sis"))

#run simulation
run_simulation(config_file = "../simulations/config/user_defined_oskar/configs/config_1.R", input_directory = "../simulations/landscape/WorldCenter_4d")


#cat mood
cat("  xD  ")

