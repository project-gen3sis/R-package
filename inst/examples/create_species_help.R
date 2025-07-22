\dontrun{
# inside a create_ancestor_species function of a config taking a space and a config
# create_species creates a new species

# define range of species for the entire world in this case lat long system
range <- c(-180, 180, -90, 90)

## select coordinates within the range stipulated above
# takes space coordinates
co <- space$coordinates
# select coordinates within the range
selection <- co[, "x"] >= range[1] &
  co[, "x"] <= range[2] &
  co[, "y"] >= range[3] &
  co[, "y"] <= range[4]
# get the initial cells
initial_cells <- rownames(co)[selection]

# call create_species
new_species <- create_species(initial_cells, config)

# extra: set local adaptation to max optimal temp equals local temp
new_species$traits[ , "temp"] <- space$environment[,"temp"]

# extra: set a certaintrait (e.g. traitX) to one on all populations of this species
new_species$traits[ , "tratiX"] <- 1
  
}
