# Hurricane Simulation to determine the AST Failure Envelope

This software performs two functions:

Part 1 of this code calculates the buoyancy and ASCE7-16 wind loadings on an AST given tank parameters (and makes assumptions if tank parameters are not given).

Part 2 calculates the loadings for tanks with a range of different diameters, wind speeds, and roof types. Comparing the results allows us to make conclusions about how a tank's size and roof type changes its susceptibility to hurricane loadings - the envelope of AST hurricane failures.

## dependencies

R package dependencies:
* here
* magrittr
* ggplot2
* RColorBrewer

## files

* RFL_overview_large.png is a high resolution version of the corresponding figure in API 656 Appendix 4, generated with envelope-plots2.R
* RFL_fixedscale_large.png is a high resolution version of the corresponding figure in API 656 Appendix 4 generated with envelope-plots2.R



* hurricaner.Rproj is the R project file. Open this up in RStudio first, then open up one of the script files to run.
* test-script.R is an example buoyancy and ASCE7-16 wind calculation
* envelope-data.R is a script to generate the simulation data over multiple diameters, wind speeds, and roof types.
* envelope-plots1.R is a script to generate some useful plots for API 656 appendix 4.
* envelope-plots2.R is a script to generate some useful plots for API 656 appendix 4.
* hurricaner_data_2021-04-08.rds is an R data file containing the results of a run of the envelope-data.R script to avoid a lengthy re-run of that code.
* src/ is a folder containing the functions used in the above script files.


## inputs

### required inputs

* D (ft diameter)
* H (ft height)
* flood_elevation (ft floodwater)
* rain (in of rainwater)
* water_speed (ft/s flood speed)
* wind_speed (mph)

### assumed inputs

you can set these in the roof list - if you don't, the following assumed values will be used

* roof_type = "cone", (roof type, "cone", "dome", or "open")
* SG = 0.7, (specific gravity of product)
* risk_category = 3, (risk category, 1, 2, or 3 (or "I", "II", or "III"))
* mu = 0.4, (friction coefficient)
* terminal_elevation = 0, (ft, terminal elevation)
* berm_height = 0, (ft, berm height)
* tank_elevation = 0, (ft, elevation of the base of the tank)
* wave_bool = FALSE, (boolean, whether breaking waves are considered or not, "TRUE" or "FALSE")
* berm_rain_ratio = 1, (ratio of berm rain water to outside rain water)
* water_drag = 2, (floodwater drag coefficient)
* exposure_category = "C" (exposure category for wind, "B", "C", or "D")
