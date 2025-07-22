# Copyright (c) 2020, ETH Zurich

# DO NOT USE ' IN THIS space META DATA
skeleton_space_metadata <- paste0(c('
gen3sis space

Version: 
1.0

Author:

', paste0("Date: \n", format(Sys.Date(), format="%d.%m.%Y")),                          
'
Spatial extent: 
   (e.g.: Theoretical Island 4-81 sites; Global; America; Japan; World latitude [-40;40] longitude [-80;80])

Spatial resolution: 
   (e.g.: none; 4 degrees)

Temporal extent: 
   (e.g.: 140-0Ma; 120-0 kyra)

Temporal resolution: 
   (e.g. 1 time-step = 1 myr; 1 time-step = 1 myr/6 ~ 170 kyr)

Environmental variables: 
   (e.g.: - Temperature (degrees Celcius) 
          - Precipitation (mm/year) )

Cost function: 
   (e.g.:  1 to all sites 
           or:
           - 1 land
           - 2 to water )

Source Data: 

Publications: 

Description: 
    (e.g.: landmasses with water as NA; full description of methods here)
'), collapse ="\n") # DO NOT REMOVE THIS ->'<-. IT IS IMPORTANT
