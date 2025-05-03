# dev

# v.1.6
  - Manually defining gen3sis$general$config_name variable is no longer 
    required. The "config_name" parameter to create_input_config(), which 
    allows users to specify the name of the configuration object to be created. 
    It is optional. If not provided, the function will either assume the name 
    based on the loaded file name or generate a random unique code. 
  - Simulation related console outputs were reworked to improve readability. 
    They are now prettier and more user friendly.
  - config_verify() output messages were reworked to improve readability. 
    They are now more informative.
  - Issue fixed: config file is now copied to output folder when using a 
    gen3sis_config object.
  - Many outdated references to "gen3sis" were substituted by "gen3sis2".
    Further work needed.
  - The documentation were updated accordingly to the changes above.

# v.1.5.12
  - restore functionality to restart a simulation from a saved state
  - if a config object is used to run a simulation, it is now required
    to define the gen3sis$general$config_name variable with the name
    of the subdirectory where the outputs of an individual simulation
    will be saved (if the config object is created from a config file,
    this variable will be automatically set from the file name)
  - fix bug in verify_config() where missing settings were not
    properly identified
    
# v.1.5.11 release 11.2023
  - fix comb phylogeny
  - color deficient, blind and B&W safe colours
  - speed-up of loop_ecology function
  - fix nexus phylo file format
  - fix accounting of extinctions times 
  - fix phylo checks if simulation did not end at t=0
  - fix package build notes citation and class comparisons

# v.1.4 release 10.2021
  - fix bracket compatibility with new R version
  - added new abundance plotting function 

# v.1.3 release 07.2021

# v.1.2 release	12.2020 

# v.1.1 release	08.2020
  
# v.1.0 release 06.2020
