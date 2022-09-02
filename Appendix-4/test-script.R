# wind calculations refresh
# test script
# AW @ PEMY

library(here)
library(magrittr)

# call source function on source code files in src
invisible(
  lapply(
    list.files(here("src")),
    function(x){source(here("src",x))}
  )
)

# note! some parameters changed for api 656 envelope, including the roof pitch
# and design flood level calculations
# and net horizontal wind force has a minimum of 0, can't go below that.

sample_roof <- list(
  # general
  D = 100,
  H = 48,
  level = 9.601965,
  roof_type = "cone",
  # rainfall
  flood_elevation = 10,
  rain = 0,
  water_speed = 0,
  # wind
  wind_speed = 120,
  SG = 1.0
)

roof <- sample_roof
roof <- hurricaner_main(roof, summary = FALSE)

out <- analysis(roof)
print(out$X)
print(out$Y)
print(out$M)