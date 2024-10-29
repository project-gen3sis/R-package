# simulate dynamic environment data
env_dym <- list(
  var1 = data.frame(x = 1:3, y = 4:6, t1 = c(NA, 2, 3), t2 = c(NA, 2, 3)),
  var2 = data.frame(x = 1:3, y = 4:6, t1 = c(1, 2, NA), t2 = c(1, NA, NA))
)
# Check if the environment data is geodynamic
is_geodynamic(env_dym)

# simulate static environment data
env_stc <- list(
  var1 = data.frame(x = 1:3, y = 4:6, t1 = c(NA, 2, 3), t2 = c(NA, 2, 3)),
  var2 = data.frame(x = 1:3, y = 4:6, t1 = c(NA, 1, 3), t2 = c(NA, 2, 3))
)
# Check if the environment data is geodynamic
is_geodynamic(env_stc)
# note that even with different values in the time columns, the function still
# returns TRUE, because geodynamic only refer to the suitable sites
